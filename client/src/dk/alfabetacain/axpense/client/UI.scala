package dk.alfabetacain.axpense.client

import frontroute.*
import frontroute.given
import dk.alfabetacain.axpense.client.Util.*
import calico.*
import calico.html.io.{ *, given }
import cats.effect.IO
import cats.effect.kernel.Resource
import fs2.concurrent.Signal
import fs2.concurrent.SignallingRef
import fs2.dom.*

object UI {

  trait OptionalIso[A, B] {
    def from(value: A): Option[B]
    def to(value: B): A
  }

  trait StringOptionalIso[A] extends OptionalIso[String, A] {
    def from(value: String): Option[A]
    def to(value: A): String
    def tpe: String
  }

  object StringOptionalIso {

    def instance[A](tpeArg: String, decode: String => Option[A], encode: A => String): StringOptionalIso[A] = {
      new StringOptionalIso[A] {
        override def from(value: String): Option[A] = decode(value)
        override def to(value: A): String           = encode(value)
        override val tpe: String                    = tpeArg
      }
    }
    given StringOptionalIso[String] = instance("text", Option.apply, identity)
  }

  def navbar(): Resource[IO, HtmlElement[IO]] = {
    navTag(
      cls        := "navbar",
      role       := List("navigation"),
      aria.label := "main navigation",
      div(
        cls := "navbar-brand",
        a(
          cls  := "navbar-item",
          href := ".",
        ),
        a(
          role               := List("button"),
          cls                := "navbar-burger",
          aria.label         := "menu",
          aria.expanded      := false,
          dataAttr("target") := "navbar",
        ),
      ),
      div(
        idAttr := "navbar",
        cls    := "navbar-menu",
        div(
          cls := "navbar-start",
          a(
            cls := "navbar-item",
            "Home",
          ),
          a(
            cls  := "navbar-item",
            href := Pages.expenses,
            "Expenses",
          ),
          a(
            cls  := "navbar-item",
            href := Pages.addExpense,
            "Add expense",
          ),
          a(
            cls  := "navbar-item",
            href := Pages.addExpenseMobile,
            "Add expense - mobile",
            // onClick --> { _.foreach(_ => BrowserNavigation.pushState(url = "/add-expense")) },
          ),
        ),
        div(
          cls := "navbar-end",
          div(
            cls := "navbar-item",
            div(
              cls := "buttons",
              a(
                cls := "button is-primary",
                "Sign up",
              ),
              a(
                cls := "button is-light",
                "Log in",
              ),
            ),
          ),
        ),
      ),
    )
  }

  def selectField[A](
      update: A => IO[Unit],
      currentValue: Signal[IO, A],
      labelValue: String,
      options: Signal[IO, Map[String, A]],
  ): Resource[IO, HtmlDivElement[IO]] = {
    div(
      cls := "field",
      label(labelValue, cls := "label"),
      div(
        cls := "control",
        div(
          cls := "select",
          select.withSelf { self =>
            (
              onChange --> (_.foreach(_ =>
                self.value.get.flatMap { value =>
                  for {
                    opts <- options.get
                    _    <- opts.get(value).fold(IO.unit)(update)
                  } yield ()
                },
              )),
              children <-- options.map { opts =>
                opts
                  .toList
                  .map { (key, _) =>
                    option(
                      selected <-- currentValue.map(v => opts.get(key) == Some(v)),
                      key,
                    )
                  }
              },
            )
          },
        ),
      ),
    )

  }

  def textField2[A: {StringOptionalIso as iso}](
      signal: SignallingRef[IO, A],
      labelValue: String,
      placeholderValue: String,
      doAutofocus: Signal[IO, Boolean],
  ): Resource[IO, HtmlDivElement[IO]] = {
    div(
      cls := "field",
      label(labelValue + " ", cls := "label"),
      div(
        cls := "control",
        input.withSelf { self =>
          (
            cls := "input",
            tpe := iso.tpe,
            autoFocus <-- doAutofocus,
            value <-- signal.map(iso.to),
            placeholder := placeholderValue,
            onInput --> (_.foreach(_ =>
              self.value.get.flatMap { value =>
                iso.from(value).fold(IO.unit)(signal.set)
              },
            )),
          )
        },
      ),
    )

  }

  def textField[A: {StringOptionalIso as iso}](
      signal: SignallingRef[IO, A],
      labelValue: String,
      placeholderValue: String,
  ): Resource[IO, HtmlDivElement[IO]] = {
    div(
      cls := "field",
      label(labelValue + " ", cls := "label"),
      div(
        cls := "control",
        input.withSelf { self =>
          (
            cls := "input",
            tpe := iso.tpe,
            value <-- signal.map(iso.to),
            placeholder := placeholderValue,
            onInput --> (_.foreach(_ =>
              self.value.get.flatMap { value =>
                iso.from(value).fold(IO.unit)(signal.set)
              },
            )),
          )
        },
      ),
    )

  }

}
