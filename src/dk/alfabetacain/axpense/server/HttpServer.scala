package dk.alfabetacain.axpense.server

import cats.effect.IO
import cats.effect.kernel.Resource
import com.comcast.ip4s.*
import dk.alfabetacain.axpense.server.events.EventSubscriber
import dk.alfabetacain.axpense.shared.AddCategoryResponse
import dk.alfabetacain.axpense.shared.AddExpenseResponse
import dk.alfabetacain.axpense.shared.Event
import dk.alfabetacain.axpense.shared.GetCategoriesResponse
import dk.alfabetacain.axpense.shared.GetExpensesResponse
import dk.alfabetacain.shared.Api
import io.circe.syntax.*
import io.odin.Logger
import org.http4s.ember.server.EmberServerBuilder
import sttp.capabilities.fs2.Fs2Streams
import sttp.model.sse.ServerSentEvent
import sttp.tapir.server.ServerEndpoint
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.server.http4s.Http4sServerOptions
import sttp.tapir.server.interceptor.log.DefaultServerLog

import scala.concurrent.duration.*

trait HttpServer {}

object HttpServer {

  private def logMaybeExcep(log: Logger[IO])(msg: String, maybeErr: Option[Throwable]): IO[Unit] = maybeErr match {
    case Some(err) => log.error(msg, err)
    case None      => log.info(msg)
  }

  def make(log: Logger[IO], db: Db, eventSubscriber: EventSubscriber): Resource[IO, HttpServer] = {
    val interp = Http4sServerInterpreter[IO](
      Http4sServerOptions
        .customiseInterceptors[IO]
        .serverLog(DefaultServerLog(
          doLogWhenReceived = msg => log.info(msg),
          doLogWhenHandled = logMaybeExcep(log),
          doLogAllDecodeFailures = logMaybeExcep(log),
          doLogExceptions = (msg, err) => log.error(msg, err),
          noLog = IO.unit,
        )).options,
    )

    val routes = interp.toRoutes(List[ServerEndpoint[Fs2Streams[IO], IO]](
      Api.addExpense.serverLogicSuccess[IO] { request =>
        db.addExpense(request.expense).map(AddExpenseResponse.apply)
      },
      Api.getExpenses.serverLogicSuccess[IO] { request =>
        db.getExpenses().map(GetExpensesResponse.apply)
      },
      Api.getCategories.serverLogicSuccess[IO] { request =>
        db.getCategories().map(GetCategoriesResponse.apply)
      },
      Api.eventsEndpoint.serverLogicSuccess { _ =>
        IO {
          eventSubscriber.subscribe()
            .map(encodeEvent)
        }
      },
      Api.addCategory.serverLogicSuccess[IO] { request =>
        db.addCategory(request.category)
          .as(AddCategoryResponse(request.category))
      },
    ))

    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(routes.orNotFound)
      .withShutdownTimeout(1.second)
      .build
      .map(_ => new HttpServer {})
  }

  private def encodeEvent(event: Event): ServerSentEvent = {
    ServerSentEvent(data = Some(event.asJson.noSpaces), eventType = Some("axpense.event"), None, None)
  }
}
