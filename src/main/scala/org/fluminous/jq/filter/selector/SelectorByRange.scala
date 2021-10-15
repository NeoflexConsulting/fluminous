package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.range.Range
final case class SelectorByRange(override val position: Int, range: Range) extends Filter {
  override val isSingleValued: Boolean = range.rangeInterval <= 0
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        result <- input.asArray
                   .map(getElementsByRange)
                   .orElse(input.asString.map(getElementsByRange))
                   .getOrElse(
                     Left(
                       EvaluationException(
                         position,
                         s"Trying to read range ${range.toString} from json of type ${input.name}"
                       )
                     )
                   )
      } yield List(result)
    }
  }

  private def getElementsByRange(seq: Vector[Json]): Either[EvaluationException, Json] = {
    val start = range.start(seq.length)
    val end   = range.end(seq.length)
    if (end < start) {
      Left(EvaluationException(position, s"Invalid range: ${range.toString}"))
    } else {
      Right(Json.fromValues(seq.slice(end, start)))
    }
  }

  private def getElementsByRange(seq: String): Either[EvaluationException, Json] = {
    val start = range.start(seq.length)
    val end   = range.end(seq.length)
    if (end < start) {
      Left(EvaluationException(position, s"Invalid range: ${range.toString}"))
    } else {
      Right(Json.fromString(seq.slice(end, start)))
    }
  }

  override val description: String = s"selector for range:  ${range.toString}"
}

object SelectorByRange {
  implicit def typeDescription: Description[SelectorByRange] = new Description[SelectorByRange] {
    override val description: String = "selector by range"
  }
}
