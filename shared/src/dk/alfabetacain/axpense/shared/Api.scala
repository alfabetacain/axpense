package dk.alfabetacain.shared

import sttp.tapir.*
import sttp.tapir.json.circe.*
import dk.alfabetacain.axpense.shared.GetExpensesResponse
import dk.alfabetacain.axpense.shared.AddExpenseRequest
import dk.alfabetacain.axpense.shared.AddExpenseResponse
import dk.alfabetacain.axpense.shared.GetCategoriesResponse

object Api {

  private val prefix = "api" / "v1"

  val getExpenses = endpoint
    .get
    .in(prefix / "expenses")
    .out(customCodecJsonBody[GetExpensesResponse])

  val addExpense = endpoint
    .post
    .in(prefix / "expenses")
    .in(customCodecJsonBody[AddExpenseRequest])
    .out(customCodecJsonBody[AddExpenseResponse])

  val getCategories = endpoint
    .get
    .in(prefix / "categories")
    .out(customCodecJsonBody[GetCategoriesResponse])

}
