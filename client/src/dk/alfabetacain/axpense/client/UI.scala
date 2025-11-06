package dk.alfabetacain.axpense.client

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

  def selectField2[A](
      signal: SignallingRef[IO, A],
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
                    _    <- opts.get(value).fold(IO.unit)(signal.set)
                  } yield ()
                },
              )),
              children <-- options.map { opts =>
                opts.toList.map {
                  (key, _) =>
                    option(
                      selected <-- signal.map(v => opts.get(key) == Some(v)),
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

  def selectField(
      signal: SignallingRef[IO, String],
      labelValue: String,
      options: List[String],
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
                  signal.set(value)
                },
              )),
              options.map { opt =>
                option(opt)
              },
            )
          },
        ),
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
