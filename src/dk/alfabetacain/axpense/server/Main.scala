package dk.alfabetacain.axpense.server

import cats.syntax.all.*
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import dk.alfabetacain.axpense.shared.Category
import cats.effect.kernel.Resource

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
      db <- Db.makeInMemory()
      _  <- Resource.eval(seedDb(db))
      _  <- HttpServer.make(db)
    } yield ()

    resources.useForever
  }

}
