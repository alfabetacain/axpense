package dk.alfabetacain.axpense.client

import scala.concurrent.duration.*
import fs2.Stream
import calico.*
import calico.html.io.{ *, given }
import calico.syntax.*
import calico.IOWebApp
import fs2.dom.HtmlElement
import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.concurrent.SignallingRef
import dk.alfabetacain.axpense.shared.Amount
import cats.syntax.all.*
import sttp.model.Uri
import dk.alfabetacain.axpense.shared.Category
import dk.alfabetacain.axpense.shared.Expense
import cats.effect.kernel.Clock
import dk.alfabetacain.axpense.shared.Date

object Root extends IOWebApp {

  private val clock: Clock[IO] = Clock[IO]

  private def amountSignal(): Resource[IO, SignallingRef[IO, Option[Amount]]] = {
    SignallingRef[IO].of(Option.empty).toResource
  }

  private def expenses(client: ApiClient): Resource[IO, SignallingRef[IO, List[Expense]]] = {
    SignallingRef[IO].of(List.empty[Expense]).toResource.flatMap { signal =>
      Stream.awakeEvery[IO](10.seconds, true)
        .foreach { _ =>
          client.getExpenses().flatMap(expenses => signal.set(expenses))
        }.compile
        .drain
        .background
        .as(signal)
    }
  }

  private def expensesView(expenses: SignallingRef[IO, List[Expense]]): Resource[IO, HtmlElement[IO]] = {
    div(
      children <-- expenses.map { expenses =>
        expenses.map { expense =>
          div(
            input.withSelf { self =>
              (
                readOnly := true,
                value := List(
                  expense.description.getOrElse(""),
                  expense.category,
                  expense.subCategory,
                  expense.amount.value.toString(),
                  expense.amount.currency,
                ).mkString(" - "),
              )
            },
          )
        }
      },
    )
  }

  private def myForm(client: ApiClient, categories: List[Category]): Resource[IO, HtmlElement[IO]] = {
    (
      SignallingRef[IO].of("").toResource,
      SignallingRef[IO].of("").toResource,
      SignallingRef[IO].of("").toResource,
      SignallingRef[IO].of("").toResource,
    ).tupled.flatMap { case (description, category, subCategory, amount) =>
      div(
        cls := "container",
        form(
          UI.textField(description, "description", "description"),
          UI.textField(category, "category", "category"),
          UI.textField(subCategory, "sub-category", "sub-category"),
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
                _ <- client.addExpense(
                  Expense(
                    Option(desc).filter(_.nonEmpty),
                    cat,
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
        expenses(client).flatMap { expenses =>
          client.getCategories().toResource.flatMap { categories =>
            div(
              cls := "section",
              myForm(client, categories),
              expensesView(expenses),
            )
          }

        }
      }
    }
  }
}
