package dk.alfabetacain.axpense.client

import cats.effect.kernel.Resource
import cats.effect.IO
import calico.*
import calico.html.io.{ *, given }
import calico.syntax.*
import calico.html.Html
import fs2.concurrent.SignallingRef
import fs2.dom.*
import fs2.concurrent.Signal

object UI {

  trait StringMapper[A] {
    def convert(value: String): Option[A]
    def tpe: String
  }

  object StringMapper {

    def instance[A](tpeArg: String, mapper: String => Option[A]): StringMapper[A] = {
      new StringMapper[A] {
        override def convert(value: String): Option[A] = mapper(value)
        override val tpe: String                       = tpeArg
      }
    }
    given StringMapper[String] = instance("text", Option.apply)
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

  def textField[A: {StringMapper as mapper}](
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
            cls         := "input",
            tpe         := mapper.tpe,
            placeholder := placeholderValue,
            onInput --> (_.foreach(_ =>
              self.value.get.flatMap { value =>
                mapper.convert(value).fold(IO.unit)(signal.set)
              },
            )),
          )
        },
      ),
    )

  }

}
