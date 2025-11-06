package dk.alfabetacain.axpense.client

import fs2.Stream
import calico.*
import calico.IOWebApp
import calico.html.io.{ *, given }
import cats.effect.IO
import cats.effect.kernel.Clock
import cats.effect.kernel.Resource
import cats.syntax.all.*
import dk.alfabetacain.axpense.client.UI.StringOptionalIso
import dk.alfabetacain.axpense.shared.Amount
import dk.alfabetacain.axpense.shared.Category
import dk.alfabetacain.axpense.shared.Date
import dk.alfabetacain.axpense.shared.Expense
import fs2.Stream
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.HtmlElement
import sttp.model.Uri
import calico.frp.given
import dk.alfabetacain.axpense.shared.Event

object Root extends IOWebApp {

  private val clock: Clock[IO] = Clock[IO]

  private def amountSignal(): Resource[IO, SignallingRef[IO, Option[Amount]]] = {
    SignallingRef[IO].of(Option.empty).toResource
  }

  private def expenses(client: ApiClient): Resource[IO, SignallingRef[IO, List[Expense]]] = {
    SignallingRef[IO].of(List.empty[Expense]).flatMap { signal =>
      client.getExpenses().flatMap(signal.set).as(signal)
    }.toResource
  }

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

  private def myForm(client: ApiClient, categories: Signal[IO, List[Category]]): Resource[IO, HtmlElement[IO]] = {
    given StringOptionalIso[Double] = StringOptionalIso.instance("number", _.toDoubleOption, _.toString)
    categories.getAndDiscreteUpdates.flatMap { (currentCategories, categoryUpdates) =>
      (
        SignallingRef[IO].of("").toResource,
        SignallingRef[IO].of(currentCategories.headOption).toResource,
        SignallingRef[IO].of("").toResource,
        SignallingRef[IO].of(0.0).toResource,
      ).tupled.flatMap { case (description, category, subCategory, amount) =>
        categoryUpdates.foreach { update =>
          category.update {
            case None  => update.headOption
            case other => update.headOption
          }
        }.compile.drain.background.flatMap { _ =>
          div(
            cls := "container",
            form(
              UI.textField(description, "description", "description"),
              UI.selectField2(
                category,
                "category",
                categories.map(cat => cat.map(c => c.name -> Some(c)).toMap),
              ),
              UI.selectField2(
                subCategory,
                "sub category",
                Signal.mapped(category)(_.fold(Map.empty[String, String])(_.subCategories.map(v => v -> v).toMap)),
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
                    desc   <- description.get
                    cat    <- category.get
                    subcat <- subCategory.get
                    am     <- amount.get
                    now    <- clock.realTimeDate
                    _      <- client.addExpense(
                      Expense(
                        Option(desc).filter(_.nonEmpty),
                        cat.fold("")(_.name),
                        subcat,
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

  private def getBaseUrl: IO[Uri] = {
    (
      window.location.protocol.get,
      window.location.host.get,
    ).tupled
      .flatMap { (protocol, host) =>
        val base = protocol + "//" + host
        IO.fromEither(
          Uri.parse(base).leftMap(err => new IllegalStateException(s"Could not parse base url: ${base} ($err)")),
        )
      }
  }

  override def render: Resource[IO, HtmlElement[IO]] = {
    getBaseUrl.toResource.flatMap { baseUri =>
      ApiClient.make(baseUri).flatMap { client =>
        val eventsStream = Stream.eval(client.getEvents()).flatten
        println("event stream ready")

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
}
