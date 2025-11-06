package dk.alfabetacain.shared

import fs2.Stream
import fs2.Pipe
import fs2.text
import sttp.tapir.*
import sttp.tapir.json.circe.*
import dk.alfabetacain.axpense.shared.GetExpensesResponse
import dk.alfabetacain.axpense.shared.AddExpenseRequest
import dk.alfabetacain.axpense.shared.AddExpenseResponse
import dk.alfabetacain.axpense.shared.GetCategoriesResponse
import sttp.model.sse.ServerSentEvent
import cats.effect.IO
import sttp.capabilities.fs2.Fs2Streams
import java.nio.charset.StandardCharsets
import dk.alfabetacain.axpense.shared.AddCategoryRequest
import dk.alfabetacain.axpense.shared.AddCategoryResponse

object Api {

  private val prefix = "api" / "v1"

  private val parseBytesToSSE: Pipe[IO, Byte, ServerSentEvent] = { stream =>
    stream
      .through(text.utf8.decode[IO])
      .through(text.lines[IO])
      .split(_.isEmpty())
      .filter(_.nonEmpty)
      .map(_.toList)
      .map(ServerSentEvent.parse)
  }

  private val serializeSSEToBytes: Pipe[IO, ServerSentEvent, Byte] = { stream =>
    stream
      .map(sse => s"${sse}\n\n")
      .through(text.utf8.encode[IO])
  }

  val getExpenses = endpoint
    .get
    .in(prefix / "expenses")
    .out(customCodecJsonBody[GetExpensesResponse])

  val addExpense = endpoint
    .post
    .in(prefix / "expenses")
    .in(customCodecJsonBody[AddExpenseRequest])
    .out(customCodecJsonBody[AddExpenseResponse])

  val getCategories = endpoint
    .get
    .in(prefix / "categories")
    .out(customCodecJsonBody[GetCategoriesResponse])

  val addCategory = endpoint
    .post
    .in(prefix / "categories")
    .in(customCodecJsonBody[AddCategoryRequest])
    .out(customCodecJsonBody[AddCategoryResponse])

  val eventsEndpoint = endpoint
    .get
    .in(prefix / "events")
    .out {
      val fs2Streams = Fs2Streams[IO]
      streamTextBody(fs2Streams)(CodecFormat.TextEventStream(), Some(StandardCharsets.UTF_8))
        .map(parseBytesToSSE)(serializeSSEToBytes)
    }

}
