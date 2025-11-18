package dk.alfabetacain.axpense.client

import frontroute.*
import frontroute.given
import dk.alfabetacain.axpense.client.Util.*
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
import fs2.dom.*

object Root extends IOWebApp {

  private val clock: Clock[IO] = Clock[IO]

  private def renderExpenses(expenses: List[Expense]): Resource[IO, HtmlElement[IO]] = {
    div(
      cls := "fixed-grid",
      div(
        cls := "grid has-2-cols",
        expenses.map { expense =>
          div(
            cls := "cell has-text-centered is-col-span-2",
            span(
              List(
                expense.description.getOrElse(""),
                expense.category,
                expense.subCategory,
                expense.amount.value.toString(),
                expense.amount.currency,
              ).mkString(" - "),
            ),
          )
        },
      ),
    )
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

  private final case class SelectedCategory(category: Category, sub: Option[String]) {
    def render: String = List(Some(category.name), sub).flatten.mkString(" / ")
  }

  enum CategoryEditing {
    case EditingCategory
    case EditingSubCategory(cat: Category)
  }

  enum AddExpenseEditing {
    case Amount
    case Category(state: CategoryEditing)
    case Note
    case None
  }

  private def focusElement(id: String)(using Window[IO]): IO[Unit] = {
    window.document.getElementById(
      id,
    ).flatMap(_.fold(IO.unit) { e =>
      e.asInstanceOf[HtmlElement[IO]].focus
    })

  }

  private def mobileAddExpense(client: ApiClient, categories: List[Category])(using
      window: Window[IO],
  ): Resource[IO, HtmlElement[IO]] = {
    given StringOptionalIso[Double] = StringOptionalIso.instance("number", _.toDoubleOption, _.toString)
    clock.realTimeDate.toResource.flatMap { now =>
      (
        makeSigRef(""),
        makeSigRef(Option.empty[SelectedCategory]),
        makeSigRef(0.0),
        makeSigRef(AddExpenseEditing.Amount),
        makeSigRef(now),
      ).tupled.flatMap { case (description, selectedCat, amount, editingCat, selectedDate) =>
        div(
          cls := "container",
          form(
            UI.dateField(selectedDate, "date"),
            UI.textField(amount, "amount", "amount"),
            div(
              cls := "field",
              label("Category"),
              autoFocus <-- editingCat.map {
                case _: AddExpenseEditing.Category => true
                case _                             => false
              },
              div(
                cls := "control",
                input(
                  cls := "input",
                  tpe := "text",
                  value <-- selectedCat.map(_.map(_.render)),
                  readOnly := true,
                  onClick --> {
                    _.foreach(_ => editingCat.set(AddExpenseEditing.Category(CategoryEditing.EditingCategory)))
                  },
                ),
              ),
            ),
            UI.textField2(
              description,
              "Note",
              "note",
              editingCat.map {
                case AddExpenseEditing.Note => true
                case _                      => false
              },
              id = Some("note"),
            ),
            div(
              children <-- editingCat.map {
                case AddExpenseEditing.Note                                      => List.empty
                case AddExpenseEditing.Amount                                    => List.empty
                case AddExpenseEditing.None                                      => List.empty
                case AddExpenseEditing.Category(CategoryEditing.EditingCategory) => List(
                    div(
                      cls := "grid box",
                      categories.map { cat =>
                        div(
                          cls := "cell button",
                          cat.name,
                          onClick --> {
                            _.foreach(_ =>
                              for {
                                isDone <- selectedCat.modify {
                                  case Some(current) if current.category.name == cat.name =>
                                    (Some(current), false)
                                  case _ => (Some(SelectedCategory(cat, None)), true)
                                }
                                _ <- if (cat.subCategories.nonEmpty) {
                                  editingCat.set(AddExpenseEditing.Category(CategoryEditing.EditingSubCategory(cat)))
                                } else {
                                  editingCat.set(AddExpenseEditing.Note) >>
                                    focusElement("note")

                                }
                              } yield (),
                            )
                          },
                        )
                      },
                    ),
                  )
                case AddExpenseEditing.Category(CategoryEditing.EditingSubCategory(cat)) =>
                  List(
                    div(
                      cls := "grid box",
                      cat.subCategories.map { sub =>
                        div(
                          cls := "cell button",
                          sub,
                          onClick --> {
                            _.foreach(_ =>
                              for {
                                _ <- selectedCat.set(Some(SelectedCategory(cat, Some(sub))))
                                _ <-
                                  editingCat.set(AddExpenseEditing.Note)
                                _ <-
                                  focusElement("note")

                              } yield (),
                            )
                          },
                        )
                      },
                    ),
                  )
              },
            ),
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
                  selectedCats <- selectedCat.get
                  am           <- amount.get
                  date         <- selectedDate.get
                  _            <- client.addExpense(
                    Expense(
                      Option(desc).filter(_.nonEmpty),
                      selectedCats.fold("")(_.category.name),
                      selectedCats.fold("")(_.sub.getOrElse("")),
                      Amount(BigDecimal(am), "DKK"),
                      Date(date.toISOString()),
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

  private def myForm(client: ApiClient, categories: List[Category]): Resource[IO, HtmlElement[IO]] = {
    given StringOptionalIso[Double] = StringOptionalIso.instance("number", _.toDoubleOption, _.toString)
    (
      SignallingRef[IO].of("").toResource,
      makeSigRef(SelectedCategories(
        categories.headOption,
        categories.headOption.flatMap(_.subCategories.headOption),
      )),
      SignallingRef[IO].of(0.0).toResource,
    ).tupled.flatMap { case (description, selectedCategories, amount) =>
      div(
        cls := "container",
        form(
          UI.textField(description, "description", "description"),
          UI.selectField[Option[Category]](
            cat => selectedCategories.update(_.updateWithCategory(cat)),
            selectedCategories.map(_.category),
            "category",
            Signal.constant[IO, Map[String, Option[Category]]](categories.map(c => c.name -> Some(c)).toMap),
            // categories.map(cat => cat.map(c => c.name -> Some(c)).toMap),
          ),
          UI.selectField[Option[String]](
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

  override def render: Resource[IO, HtmlElement[IO]] = {
    ApiClient.make(window).flatMap { client =>
      given Window[IO] = window
      val eventsStream = Stream.eval(client.getEvents()).flatten

      val expensesStream = (Stream.eval(client.getExpenses()) ++ eventsStream
        .filter(_ == Event.ExpensesUpdated)
        .evalMap(_ => client.getExpenses()))
        .hold1Resource

      val categoriesStream = (Stream.eval(client.getCategories()) ++ eventsStream
        .filter(_ == Event.CategoriesUpdated)
        .evalMap(_ => client.getCategories()))
        .hold1Resource

      (expensesStream, categoriesStream)
        .tupled
        .flatMap { (expensesSignal, categoriesSignal) =>
          routes(
            pathPrefix("axpense") {
              div(
                div(
                  cls := "section",
                  UI.navbar(),
                ),
                path(Pages.addExpense) {
                  div(
                    cls := "section",
                    children <-- categoriesSignal
                      .map(categories => List(myForm(client, categories))),
                  )

                },
                path(Pages.addExpenseMobile) {
                  div(
                    cls := "section",
                    children <-- categoriesSignal
                      .map(categories => List(mobileAddExpense(client, categories))),
                  )

                },
                path(Pages.expenses) {
                  div(
                    cls := "section",
                    children <--
                      expensesSignal
                        .map(expenses => List(renderExpenses(expenses))),
                  )

                },
                LinkHandler,
              )
            },
          )
        }

    }

  }
}
