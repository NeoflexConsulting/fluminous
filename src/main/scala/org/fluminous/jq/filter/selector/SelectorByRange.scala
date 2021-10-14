package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class SelectorByRange(override val position: Int, firstIndex: Option[Int], lastIndex: Option[Int])
    extends Filter {
  override val isSingleValued: Boolean = rangeInterval <= 0
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        jsonArray <- input.asArray.toRight(
                      EvaluationException(
                        position,
                        s"Trying to read range $rangeAsString from json of type ${input.name}"
                      )
                    )
        element <- getElementsByRange(jsonArray)
      } yield List(Json.fromValues(element))
    }
  }

  private def getElementsByRange(jsonArray: Vector[Json]): Either[EvaluationException, Vector[Json]] = {
    val firstIndexNumber  = firstIndex.getOrElse(0)
    val lastIndexNumber   = lastIndex.getOrElse(jsonArray.length)
    val firstNaturalIndex = if (firstIndexNumber >= 0) firstIndexNumber else jsonArray.length + firstIndexNumber
    val lastNaturalIndex  = if (lastIndexNumber >= 0) lastIndexNumber else jsonArray.length + lastIndexNumber
    if (lastNaturalIndex < firstNaturalIndex) {
      Left(EvaluationException(position, s"Invalid range: $rangeAsString"))
    } else {
      Right(jsonArray.slice(firstNaturalIndex, lastNaturalIndex))
    }
  }

  private def rangeAsString: String = {
    s"${firstIndex.map(_.toString).getOrElse("")}:${lastIndex.map(_.toString).getOrElse("")}"
  }

  private def rangeInterval: Int = {
    val f = firstIndex.getOrElse(0)
    val l = lastIndex.getOrElse(f + 1)
    l - f
  }

  override val description: String = s"selector for range: $rangeAsString"
}

object SelectorByRange {
  implicit def typeDescription: Description[SelectorByRange] = new Description[SelectorByRange] {
    override val description: String = "selector by range"
  }
}
