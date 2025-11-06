package dk.alfabetacain.axpense.server

import cats.syntax.all.*
import cats.effect.IO
import dk.alfabetacain.axpense.shared.Expense
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import dk.alfabetacain.axpense.shared.Category
import dk.alfabetacain.axpense.server.events.EventPublisher
import dk.alfabetacain.axpense.server.events.EventHandler
import dk.alfabetacain.axpense.shared.Event

trait Db {
  def addExpense(expense: Expense): IO[Expense]
  def getExpenses(): IO[List[Expense]]
  def getCategories(): IO[List[Category]]
  def addCategory(category: Category): IO[Unit]
}

private final class InMemoryDb(
    ref: Ref[IO, List[Expense]],
    categories: Ref[IO, List[Category]],
    eventPublisher: EventPublisher,
) extends Db {

  override def addExpense(expense: Expense): IO[Expense] = {
    ref.update(expense :: _)
      .as(expense)
      .flatTap(_ => eventPublisher.publish(Event.ExpensesUpdated))
  }

  override def getExpenses(): IO[List[Expense]] = {
    ref.get
  }

  override def getCategories(): IO[List[Category]] = {
    categories.get
  }

  override def addCategory(category: Category): IO[Unit] = {
    categories.update(category :: _) >> eventPublisher.publish(Event.CategoriesUpdated)
  }

}

object Db {

  def makeInMemory(eventPublisher: EventPublisher): Resource[IO, Db] = {
    Resource.eval(
      (
        Ref.of[IO, List[Expense]](List.empty),
        Ref.of[IO, List[Category]](List.empty),
      ).tupled,
    ).map { (expenses, categories) => new InMemoryDb(expenses, categories, eventPublisher) }
  }
}
