package dk.alfabetacain.axpense.server

import cats.effect.IO
import io.odin.slf4j.OdinLoggerServiceProvider
import io.odin.Logger
import cats.effect.std.Dispatcher
import cats.effect.kernel.Sync
import io.odin.*

class OdinSlf4jBridge extends OdinLoggerServiceProvider[IO] {

  override implicit def F: Sync[IO] = IO.asyncForIO

  override implicit val dispatcher: Dispatcher[IO] =
    Dispatcher.sequential[IO].allocated.unsafeRunSync()(using cats.effect.unsafe.implicits.global)._1

  override def loggers: PartialFunction[String, Logger[IO]] = {
    case _ =>
      consoleLogger[IO]()
  }

}
