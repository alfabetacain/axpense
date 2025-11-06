package dk.alfabetacain.axpense.server

import io.circe.syntax.*
import scala.concurrent.duration.*
import com.comcast.ip4s.*
import cats.effect.kernel.Resource
import cats.effect.IO
import org.http4s.ember.server.EmberServerBuilder
import sttp.tapir.server.http4s.Http4sServerInterpreter
import dk.alfabetacain.shared.Api
import dk.alfabetacain.axpense.shared.AddExpenseResponse
import dk.alfabetacain.axpense.shared.GetExpensesResponse
import org.http4s.HttpRoutes
import org.http4s.server.Router
import dk.alfabetacain.axpense.shared.GetCategoriesResponse
import dk.alfabetacain.axpense.server.events.EventSubscriber
import dk.alfabetacain.axpense.shared.Event
import sttp.model.sse.ServerSentEvent
import sttp.tapir.server.http4s.Http4sServerSentEvents
import sttp.tapir.server.ServerEndpoint
import sttp.capabilities.fs2.Fs2Streams
import dk.alfabetacain.axpense.shared.AddCategoryResponse

trait HttpServer {}

object HttpServer {

  def make(db: Db, eventSubscriber: EventSubscriber): Resource[IO, HttpServer] = {
    val interp = Http4sServerInterpreter[IO]()

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
