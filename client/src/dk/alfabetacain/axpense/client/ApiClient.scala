package dk.alfabetacain.axpense.client

import cats.effect.IO
import dk.alfabetacain.axpense.shared.Expense
import dk.alfabetacain.axpense.shared.Category
import cats.effect.kernel.Resource
import sttp.client4.impl.cats.FetchCatsBackend
import sttp.tapir.client.sttp4.SttpClientInterpreter
import dk.alfabetacain.shared.Api
import sttp.model.Uri
import dk.alfabetacain.axpense.shared.AddExpenseRequest

trait ApiClient {

  def getExpenses(): IO[List[Expense]]

  def addExpense(expense: Expense): IO[Expense]

  def getCategories(): IO[List[Category]]
}

object ApiClient {

  def make(baseUrl: Uri): Resource[IO, ApiClient] = {
    Resource.make(IO(FetchCatsBackend[IO]()))(_.close())
      .map { backend =>
        val interp = SttpClientInterpreter()

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
        }
      }
  }
}
