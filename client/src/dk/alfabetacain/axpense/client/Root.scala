package dk.alfabetacain.axpense.client

import calico.*
import calico.IOWebApp
import calico.frp.given
import calico.html.io.{ *, given }
import cats.effect.IO
import cats.effect.kernel.Clock
import cats.effect.kernel.Resource
import cats.syntax.all.*
import dk.alfabetacain.axpense.client.UI.StringOptionalIso
import dk.alfabetacain.axpense.shared.Amount
import dk.alfabetacain.axpense.shared.Category
import dk.alfabetacain.axpense.shared.Date
import dk.alfabetacain.axpense.shared.Event
import dk.alfabetacain.axpense.shared.Expense
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement

object Root extends IOWebApp {

  private val clock: Clock[IO] = Clock[IO]

  private def renderExpenses(expenses: Signal[IO, List[Expense]]): Resource[IO, HtmlElement[IO]] = {
    div(
      cls := "fixed-grid",
      div(
        cls := "grid has-2-cols",
        children <-- expenses.map { expenses =>
          expenses.map { expense =>
            div(
              cls := "cell has-text-centered is-col-span-2",
              span(
                // cls := "input",
                List(
                  expense.description.getOrElse(""),
                  expense.category,
                  expense.subCategory,
                  expense.amount.value.toString(),
                  expense.amount.currency,
                ).mkString(" - "),
              ),
            )
          }
        },
      ),
    )
  }

  private def makeSigRef[A](initial: A): Resource[IO, SignallingRef[IO, A]] = {
    SignallingRef[IO].of(initial).toResource
  }

  private final case class SelectedCategories(category: Option[Category], subCategory: Option[String]) {

    def updateWithCategories(categories: List[Category]): SelectedCategories = {
      category.flatMap(cat => categories.find(_.name == cat.name)) match {
        case None =>
          updateWithCategory(None)
        case Some(newCat) =>
          updateWithCategory(Some(newCat))
      }
    }

    def updateWithCategory(cat: Option[Category]): SelectedCategories = {
      SelectedCategories(
        cat,
        for {
          actualCategory <- cat
          subCategories = actualCategory.subCategories
          value <- subCategory.flatMap(sub => subCategories.find(_ == sub)).orElse(subCategories.headOption)
        } yield value,
      )
    }

    def updateWithSubcategory(sub: Option[String]): SelectedCategories = {
      sub match {
        case Some(s) if category.exists(_.subCategories.contains(s)) =>
          SelectedCategories(category, sub)
        case _ => SelectedCategories(category, None)
      }
    }
  }

  private def myForm(client: ApiClient, categories: Signal[IO, List[Category]]): Resource[IO, HtmlElement[IO]] = {
    given StringOptionalIso[Double] = StringOptionalIso.instance("number", _.toDoubleOption, _.toString)
    categories.getAndDiscreteUpdates.flatMap { (currentCategories, categoryUpdates) =>
      (
        SignallingRef[IO].of("").toResource,
        makeSigRef(SelectedCategories(
          currentCategories.headOption,
          currentCategories.headOption.flatMap(_.subCategories.headOption),
        )),
        SignallingRef[IO].of(0.0).toResource,
      ).tupled.flatMap { case (description, selectedCategories, amount) =>
        categoryUpdates.foreach { update =>
          selectedCategories.update(_.updateWithCategories(update))
        }.compile.drain.background.flatMap { _ =>
          div(
            cls := "container",
            form(
              UI.textField(description, "description", "description"),
              UI.selectField2[Option[Category]](
                cat => selectedCategories.update(_.updateWithCategory(cat)),
                selectedCategories.map(_.category),
                "category",
                categories.map(cat => cat.map(c => c.name -> Some(c)).toMap),
              ),
              UI.selectField2[Option[String]](
                sub => selectedCategories.update(_.updateWithSubcategory(sub)),
                selectedCategories.map(_.subCategory),
                "sub category",
                selectedCategories.map(
                  _.category.map(_.subCategories.map(c => c -> Some(c)).toMap).getOrElse(Map.empty),
                ),
              ),
              UI.textField(amount, "amount", "amount"),
              div(
                cls := "control",
                input(
                  tpe := "submit",
                  cls := "button is-link",
                  "Submit",
                ),
              ),
              action := "javascript:void(0);",
              onSubmit --> { event =>
                event.foreach { _ =>
                  for {
                    desc         <- description.get
                    selectedCats <- selectedCategories.get
                    am           <- amount.get
                    now          <- clock.realTimeDate
                    _            <- client.addExpense(
                      Expense(
                        Option(desc).filter(_.nonEmpty),
                        selectedCats.category.fold("")(_.name),
                        selectedCats.subCategory.getOrElse(""),
                        Amount(BigDecimal(am), "DKK"),
                        Date(now.toISOString()),
                      ),
                    )
                  } yield ()
                }
              },
            ),
          )

        }
      }
    }
  }

  override def render: Resource[IO, HtmlElement[IO]] = {
    ApiClient.make(window).flatMap { client =>
      val eventsStream = Stream.eval(client.getEvents()).flatten

      val expensesStream = Stream.eval(client.getExpenses()) ++ eventsStream
        .filter(_ == Event.ExpensesUpdated)
        .evalMap(_ => client.getExpenses())

      val categoriesStream = Stream.eval(client.getCategories()) ++ eventsStream
        .filter(_ == Event.CategoriesUpdated)
        .evalMap(_ => client.getCategories())

      (expensesStream.hold1Resource, categoriesStream.hold1Resource)
        .parTupled
        .flatMap { (expenses, categories) =>
          div(
            div(
              cls := "section",
              myForm(client, categories),
            ),
            div(cls := "section", renderExpenses(expenses)),
          )
        }

    }

  }
}
