package dk.alfabetacain.axpense.server.events

import fs2.Stream
import cats.effect.IO
import fs2.concurrent.Topic
import cats.effect.kernel.Resource

trait EventPublisher {
  def publish(event: Event): IO[Unit]
}

trait EventSubscriber {
  def subscribe(): Stream[IO, Event]
}

trait EventHandler extends EventPublisher with EventSubscriber {}

enum Event {
  case CategoriesUpdated
}

private final class EventHandlerImpl(topic: Topic[IO, Event]) extends EventHandler {

  override def subscribe(): Stream[IO, Event] = {
    // maybe this should be lossy somehow
    topic.subscribe(10)
  }

  override def publish(event: Event): IO[Unit] = {
    topic
      .publish1(event)
      .void
  }
}

object EventHandler {

  def make(): Resource[IO, EventHandler] = {
    Topic[IO, Event].toResource.map { topic =>
      new EventHandlerImpl(topic)
    }
  }
}
