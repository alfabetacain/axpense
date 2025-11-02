package dk.alfabetacain.axpense.server

import cats.syntax.all.*
import cats.effect.IO
import dk.alfabetacain.axpense.shared.Expense
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import dk.alfabetacain.axpense.shared.Category

trait Db {
  def addExpense(expense: Expense): IO[Expense]
  def getExpenses(): IO[List[Expense]]
  def getCategories(): IO[List[Category]]
  def addCategory(category: Category): IO[Unit]
}

private final class InMemoryDb(ref: Ref[IO, List[Expense]], categories: Ref[IO, List[Category]]) extends Db {

  override def addExpense(expense: Expense): IO[Expense] = {
    ref.update(expense :: _)
      .as(expense)
  }

  override def getExpenses(): IO[List[Expense]] = {
    ref.get
  }

  override def getCategories(): IO[List[Category]] = {
    categories.get
  }

  override def addCategory(category: Category): IO[Unit] = {
    categories.update(category :: _)
  }

}

object Db {

  def makeInMemory(): Resource[IO, Db] = {
    Resource.eval(
      (
        Ref.of[IO, List[Expense]](List.empty),
        Ref.of[IO, List[Category]](List.empty),
      ).tupled,
    ).map { (expenses, categories) => new InMemoryDb(expenses, categories) }
  }
}
