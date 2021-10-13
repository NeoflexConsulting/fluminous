package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter

final case class SelectorByIndex(override val position: Int, index: Int) extends Filter {
  override def transformSingle(input: Json): Either[EvaluationException, Json] = {
    if (input.isNull) {
      Right(input)
    } else {
      for {
        jsonArray <- input.asArray.toRight(
                      EvaluationException(position, s"Trying to read $index element from json of type ${input.name}")
                    )
        element <- getElementByIndex(jsonArray)
      } yield element
    }
  }

  private def getElementByIndex(jsonArray: Vector[Json]): Either[EvaluationException, Json] = {
    val naturalIndex = if (index >= 0) index else jsonArray.length + index
    jsonArray
      .lift(naturalIndex)
      .toRight(EvaluationException(position, s"Invalid index $index for array of length ${jsonArray.length}"))
  }

  override val description: String = s"selector for index: $index"
}

object SelectorByIndex {
  implicit def typeDescription: Description[SelectorByIndex] = new Description[SelectorByIndex] {
    override val description: String = "selector by index"
  }
}
