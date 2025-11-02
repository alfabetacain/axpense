package dk.alfabetacain.axpense.client

import cats.effect.kernel.Resource
import cats.effect.IO
import calico.*
import calico.html.io.{ *, given }
import calico.syntax.*
import calico.html.Html
import fs2.concurrent.SignallingRef
import fs2.dom.*

object UI {

  def textField(
      signal: SignallingRef[IO, String],
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
            placeholder := placeholderValue,
            onInput --> (_.foreach(_ => self.value.get.flatMap(signal.set))),
          )
        },
      ),
    )

  }
}
