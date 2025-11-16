package dk.alfabetacain.axpense.client

import cats.effect.IO
import fs2.concurrent.SignallingRef
import cats.effect.kernel.Resource

object Util {

  def makeSigRef[A](initial: A): Resource[IO, SignallingRef[IO, A]] = {
    SignallingRef[IO].of(initial).toResource
  }
}
