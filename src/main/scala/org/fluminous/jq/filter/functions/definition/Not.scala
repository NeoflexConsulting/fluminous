package org.fluminous.jq.filter.functions.definition

import io.circe.Json
import org.fluminous.jq.EvaluationException
import io.circe.syntax._
import org.fluminous.jq.filter.Filter

object Not extends JqFunction {
  override val name: String               = "not"
  val minParameters: Int = 0
  val maxParameters: Int = 0
  val defaultParameters: Map[Int, Filter] = Map.empty
  override val isSingledValue: Boolean    = true
  override def invoke(input: Json, parameters: List[Filter], position: Int): Either[EvaluationException, List[Json]] = {
    input.asBoolean
      .map(b => List((!b).asJson))
      .toRight(EvaluationException(position, s"""Could not apply function 'not' to ${input.name}"""))
  }

}
