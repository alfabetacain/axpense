package dk.alfabetacain.axpense.shared

import io.circe.Codec
import sttp.tapir.Schema

final case class Amount(
    /**
      * BigDecimal representation
      */
    value: BigDecimal,
    currency: String,
) derives Codec.AsObject, Schema

final case class Date(value: String) derives Codec.AsObject, Schema

final case class Expense(
    description: Option[String],
    category: String,
    subCategory: String,
    amount: Amount,
    date: Date,
) derives Codec.AsObject, Schema

final case class GetExpensesResponse(expenses: List[Expense]) derives Codec.AsObject, Schema

final case class AddExpenseRequest(expense: Expense) derives Codec.AsObject, Schema

final case class AddExpenseResponse(expense: Expense) derives Codec.AsObject, Schema

final case class Category(name: String, subCategories: List[String]) derives Codec.AsObject, Schema

final case class GetCategoriesResponse(categories: List[Category]) derives Codec.AsObject, Schema
