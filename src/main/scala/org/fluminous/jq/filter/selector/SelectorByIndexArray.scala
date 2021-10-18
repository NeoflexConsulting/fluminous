package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import cats.syntax.traverse._

case class SelectorByIndexArray(
  override val position: Int,
  indexArray: List[Filter],
  parentFieldName: Option[String] = None)
    extends Filter
    with SelectorFunctions {
  override val isSingleValued: Boolean = indexArray.length > 1

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    if (input.isNull) {
      Right(List(input))
    } else {
      for {
        childJson <- jsonByFieldName(input, parentFieldName)
        jsonArray <- childJson.asArray.toRight(
                      EvaluationException(position, s"Trying to read elements by index from json of type ${input.name}")
                    )
        indexes <- jsonArrayToIndexArray(input)
      } yield indexes.map(getElementByIndex(jsonArray, _))
    }
  }

  private def jsonArrayToIndexArray(input: Json): Either[EvaluationException, List[Int]] = {
    for {
      evaluatedIndexJsons <- indexArray.map(_.transform(input)).flatSequence
      indexes <- evaluatedIndexJsons
                  .map(
                    _.asNumber.flatMap(_.toInt).toRight(EvaluationException(position, s"Not all indexes are integers"))
                  )
                  .sequence
    } yield indexes
  }

  override val description: String = s"selector by index array"
}

object SelectorByIndexArray {
  implicit def typeDescription: Description[SelectorByIndexArray] = new Description[SelectorByIndexArray] {
    override val description: String = "selector by index array"
  }
}
