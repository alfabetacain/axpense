package dk.alfabetacain.axpense.server

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

trait HttpServer {}

object HttpServer {

  def make(db: Db): Resource[IO, HttpServer] = {
    val interp = Http4sServerInterpreter[IO]()

    val routes = interp.toRoutes(List(
      Api.addExpense.serverLogicSuccess[IO] { request =>
        db.addExpense(request.expense).map(AddExpenseResponse.apply)
      },
      Api.getExpenses.serverLogicSuccess[IO] { request =>
        db.getExpenses().map(GetExpensesResponse.apply)
      },
      Api.getCategories.serverLogicSuccess[IO] { request =>
        db.getCategories().map(GetCategoriesResponse.apply)
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
}
