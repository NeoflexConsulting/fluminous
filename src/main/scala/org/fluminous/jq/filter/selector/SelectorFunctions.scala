package org.fluminous.jq.filter.selector

import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.Filter

trait SelectorFunctions {
  self: Filter =>

  protected def getElementByIndex(jsonArray: Vector[Json], index: Int): Json = {
    val naturalIndex = if (index >= 0) index else jsonArray.length + index
    jsonArray
      .lift(naturalIndex)
      .getOrElse(Null)
  }

  protected def jsonByFieldName(input: Json, parentFieldName: Option[String]): Either[EvaluationException, Json] = {
    parentFieldName.map(readField(input)).getOrElse(Right(input))
  }

  private def readField(input: Json)(parentFieldName: String): Either[EvaluationException, Json] = {
    for {
      jsonObject <- input.asObject.toRight(
                     EvaluationException(
                       position,
                       s"Trying to read field $parentFieldName from json of type ${input.name}"
                     )
                   )
    } yield jsonObject(parentFieldName).getOrElse(Null)
  }
}
