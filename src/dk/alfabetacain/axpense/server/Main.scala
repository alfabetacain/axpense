package dk.alfabetacain.axpense.server

import cats.syntax.all.*
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import dk.alfabetacain.axpense.shared.Category
import cats.effect.kernel.Resource
import dk.alfabetacain.axpense.server.events.EventHandler

object Main extends IOApp {

  private def seedDb(db: Db): IO[Unit] = {
    List(
      Category(
        "Needs",
        List("Groceries", "Rent"),
      ),
      Category(
        "Wants",
        List("Eating out", "Board games"),
      ),
    ).traverse_(db.addCategory)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val resources = for {
      eventHandler <- EventHandler.make()
      db           <- Db.makeInMemory(eventHandler)
      _            <- Resource.eval(seedDb(db))
      _            <- HttpServer.make(db, eventHandler)
    } yield ()

    resources.useForever
  }

}
