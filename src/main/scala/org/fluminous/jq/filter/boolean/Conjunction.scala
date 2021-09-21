package org.fluminous.jq.filter.boolean

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException, FoldFunctions }
import org.fluminous.jq.filter.Filter
import io.circe.syntax._

final case class Conjunction(override val position: Int, values: List[Filter]) extends Filter with FoldFunctions {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    foldBoolean(values)(_.transform(input).flatMap(castAsBoolean(position)))(And).map(_.asJson)
  }
  override val description: String = Conjunction.typeDescription.description

  private def castAsBoolean(position: Int)(json: Json): Either[EvaluationException, Boolean] = {
    json.asNull
      .fold(json)(_ => Json.False)
      .asBoolean
      .toRight(EvaluationException(position, "Not boolean value on logical operation"))
  }
}

object Conjunction {
  implicit def typeDescription: Description[Conjunction] = new Description[Conjunction] {
    override val description: String = "conjunction"
  }
}
