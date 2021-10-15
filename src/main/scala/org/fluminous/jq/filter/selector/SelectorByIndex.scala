package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class SelectorByIndex(override val position: Int, index: Int) extends Filter {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        jsonArray <- input.asArray.toRight(
                      EvaluationException(position, s"Trying to read $index element from json of type ${input.name}")
                    )
      } yield List(getElementByIndex(jsonArray))
    }
  }

  private def getElementByIndex(jsonArray: Vector[Json]): Json = {
    val naturalIndex = if (index >= 0) index else jsonArray.length + index
    jsonArray
      .lift(naturalIndex)
      .getOrElse(Null)
  }

  override val description: String = s"selector for index: $index"
}

object SelectorByIndex {
  implicit def typeDescription: Description[SelectorByIndex] = new Description[SelectorByIndex] {
    override val description: String = "selector by index"
  }
}
