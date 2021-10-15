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
        jsonArray <- input.asArray.toRight(
                      EvaluationException(
                        position,
                        s"Trying to read range ${range.toString} from json of type ${input.name}"
                      )
                    )
        element <- getElementsByRange(jsonArray)
      } yield List(Json.fromValues(element))
    }
  }

  private def getElementsByRange(jsonArray: Vector[Json]): Either[EvaluationException, Vector[Json]] = {
    val start = range.start(jsonArray)
    val end   = range.end(jsonArray)
    if (end < start) {
      Left(EvaluationException(position, s"Invalid range: ${range.toString}"))
    } else {
      Right(jsonArray.slice(end, start))
    }
  }

  override val description: String = s"selector for range:  ${range.toString}"
}

object SelectorByRange {
  implicit def typeDescription: Description[SelectorByRange] = new Description[SelectorByRange] {
    override val description: String = "selector by range"
  }
}
