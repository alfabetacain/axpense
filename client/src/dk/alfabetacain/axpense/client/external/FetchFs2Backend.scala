package dk.alfabetacain.axpense.external

import fs2.Stream
import cats.syntax.all.*
import cats.effect.syntax.all.*
import sttp.client4.fetch.FetchOptions
import org.scalajs.dom.experimental.{ BodyInit, Request => FetchRequest, Response => FetchResponse }
import sttp.client4.fetch.AbstractFetchBackend
import sttp.capabilities.fs2.Fs2Streams
import cats.effect.kernel.Async
import sttp.client4.impl.cats.CatsMonadAsyncError
import sttp.capabilities.Streams
import sttp.ws.WebSocketFrame.Data
import sttp.ws.WebSocket
import sttp.ws.WebSocketFrame
import scala.scalajs.js
import sttp.client4.internal.ConvertFromFuture
import scala.concurrent.Future
import sttp.client4.WebSocketStreamBackend
import cats.effect.kernel.Resource
import scala.scalajs.js.typedarray.*
import org.scalajs.dom.ReadableStream
import fs2.Chunk
import fs2.concurrent.Signal
import cats.effect.kernel.Deferred

class FetchFs2Backend[F[_]: Async] private (fetchOptions: FetchOptions, customizeRequest: FetchRequest => FetchRequest)
    extends AbstractFetchBackend[F, Fs2Streams[F]](fetchOptions, customizeRequest, new CatsMonadAsyncError)
    with WebSocketStreamBackend[F, Fs2Streams[F]] {

  override val streams: Streams[Fs2Streams[F]] = Fs2Streams[F]

  override protected def addCancelTimeoutHook[T](result: F[T], cancel: () => Unit, cleanup: () => Unit): F[T] = {
    println("addCancelTimeoutHook")
    result.map { v => println(s"doing something with $v"); v }.onCancel(Async[F].delay(cancel())).guarantee(
      Async[F].delay(cleanup()),
    )
  }

  override protected def compileWebSocketPipe(
      ws: WebSocket[F],
      pipe: streams.Pipe[Data[_], WebSocketFrame],
  ): F[Unit] = {
    println("compileWebSocketPipe")
    ???
  }

  override protected def handleResponseAsStream(response: FetchResponse)
      : F[(streams.BinaryStream, () => F[Unit])] = {
    println("handleResponseAsStream")
    Deferred[F, Either[Throwable, Unit]].map { deferred =>
      lazy val reader = response.body.getReader()
      def read()      = Async[F].fromFuture(Async[F].delay(reader.read().toFuture))

      val cancel = Async[F].async[Unit] { callback =>
        Async[F].delay {
          reader.cancel("Response body reader cancelled").`then`(
            res => callback(Right(res)),
            {
              case err: Throwable =>
                callback(Left(err))
              case other =>
                callback(Left(scalajs.js.JavaScriptException(other)))
            },
          )
        }.as(None)
      }
      val stream = Stream.unfoldChunkEval(()) { case () =>
        read()
          .map { chunk =>
            if (chunk.done) {
              Option.empty
            } else {
              val bytes = new Int8Array(chunk.value.buffer).toArray
              Option((Chunk.array(bytes), ()))
            }
          }
      }.onComplete(Stream.eval_(cancel))
      (stream.asInstanceOf[streams.BinaryStream], () => deferred.complete(Right(())).void)
    }
  }

  override protected def handleStreamBody(s: streams.BinaryStream): F[js.UndefOr[BodyInit]] = {
    println("handleStreamBody")
    s
      .asInstanceOf[Stream[F, Byte]]
      .chunks
      .fold(Chunk.empty[Byte])(_ ++ _)
      .compile
      .last
      .map {
        case None      => js.undefined
        case Some(res) => res.toArray.toTypedArray
      }
  }

  override def convertFromFuture: ConvertFromFuture[F] = {
    new ConvertFromFuture[F] {
      override def apply[T](f: Future[T]): F[T] = Async[F].fromFuture(monad.unit(f))
    }
  }
}

object FetchFs2Backend {

  def resource[F[_]: Async](
      fetchOptions: FetchOptions = FetchOptions.Default,
      customizeRequest: FetchRequest => FetchRequest = identity,
  ): Resource[F, WebSocketStreamBackend[F, Fs2Streams[F]]] = {
    Resource.pure(new FetchFs2Backend[F](fetchOptions, customizeRequest))
  }
}
