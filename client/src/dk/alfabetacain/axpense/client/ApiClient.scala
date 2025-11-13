package dk.alfabetacain.axpense.client

import cats.syntax.all.*
import cats.effect.IO
import cats.effect.kernel.Resource
import dk.alfabetacain.axpense.external.FetchFs2Backend
import dk.alfabetacain.axpense.shared.AddExpenseRequest
import dk.alfabetacain.axpense.shared.Category
import dk.alfabetacain.axpense.shared.Event
import dk.alfabetacain.axpense.shared.Expense
import dk.alfabetacain.shared.Api
import fs2.Stream
import io.circe.parser.decode as circeDecode
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.Uri
import sttp.model.sse.ServerSentEvent
import sttp.tapir.client.sttp4.SttpClientInterpreter
import sttp.tapir.client.sttp4.stream.StreamSttpClientInterpreter
import fs2.dom.Window
import scala.util.Try
import fs2.concurrent.Topic
import cats.effect.Ref

trait ApiClient {

  def getExpenses(): IO[List[Expense]]

  def addExpense(expense: Expense): IO[Expense]

  def getCategories(): IO[List[Category]]

  def getEvents(): IO[Stream[IO, Event]]
}

private class InMemoryApiClient(
    expensesRef: Ref[IO, List[Expense]],
    categoriesRef: Ref[IO, List[Category]],
    topic: Topic[IO, Event],
) extends ApiClient {

  override def getExpenses(): IO[List[Expense]] = expensesRef.get

  override def addExpense(expense: Expense): IO[Expense] = for {
    _ <- expensesRef.update(expense :: _)
    _ <- topic.publish1(Event.ExpensesUpdated)
  } yield expense

  override def getCategories(): IO[List[Category]] = categoriesRef.get

  override def getEvents(): IO[Stream[IO, Event]] =
    IO {
      topic
        .subscribeUnbounded
    }

}

object ApiClient {

  private def makeInMemory(): Resource[IO, ApiClient] = {
    (
      Ref[IO].of(List.empty[Expense]).toResource,
      Ref[IO].of(List(
        Category("animal", List("fish", "cat", "dog")),
        Category("wants", List("board games", "restaurant", "taxi")),
      )).toResource,
      Topic[IO, Event].toResource,
    ).mapN { (expensesRef, categoriesRef, eventTopic) =>
      new InMemoryApiClient(expensesRef, categoriesRef, eventTopic)
    }
  }

  def make(window: Window[IO]): Resource[IO, ApiClient] = {
    Try(scalajs.js.Dynamic.global.isGithubPages)
      .toOption
      .forall(_ == null) match {
      case true =>
        IO.println("making in memory").toResource >>
          makeInMemory()
      case false =>
        (
          window.location.protocol.get,
          window.location.host.get,
        ).tupled
          .flatMap { (protocol, host) =>
            val base = protocol + "//" + host
            IO.fromEither(
              Uri.parse(base).leftMap(err => new IllegalStateException(s"Could not parse base url: ${base} ($err)")),
            )
          }.toResource
          .flatMap { baseUrl =>
            FetchFs2Backend.resource[IO]()
              .map { backend =>
                val interp       = SttpClientInterpreter()
                val streamInterp = StreamSttpClientInterpreter()

                new ApiClient {
                  override def addExpense(expense: Expense): IO[Expense] = {
                    interp.toRequestThrowErrors(Api.addExpense, Some(baseUrl))
                      .apply(AddExpenseRequest(expense))
                      .send(backend)
                      .map(_.body.expense)
                  }
                  override def getCategories(): IO[List[Category]] = {
                    interp.toRequestThrowErrors(Api.getCategories, Some(baseUrl))
                      .apply(())
                      .send(backend)
                      .map(_.body.categories)
                  }
                  override def getExpenses(): IO[List[Expense]] = {
                    interp.toRequestThrowErrors(Api.getExpenses, Some(baseUrl))
                      .apply(())
                      .send(backend)
                      .map(_.body.expenses)
                  }

                  override def getEvents(): IO[Stream[IO, Event]] = {
                    println("get events")
                    val req =
                      streamInterp.toRequestThrowErrors[Unit, Unit, Stream[IO, ServerSentEvent], Fs2Streams[IO]](
                        Api.eventsEndpoint,
                        Some(baseUrl),
                      )
                    val applied = req.apply(())
                    println("sending request...")
                    applied
                      .send(backend)
                      .map { response =>
                        println(s"got response: $response")
                        response
                          .body
                          .evalMapFilter { event =>
                            event.data.map(circeDecode[Event]) match {
                              case None               => IO.println(s"Missing data from '$event'").as(Option.empty)
                              case Some(Left(err))    => IO.println(s"Failed to decode '$event': $err").as(Option.empty)
                              case Some(Right(value)) => IO.pure(Option(value))
                            }
                          }
                      }
                  }
                }
              }
          }
    }
  }
}
