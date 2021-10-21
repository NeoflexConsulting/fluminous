package org.fluminous.jq.filter.functions.definition

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.Filter

object Length extends JqFunction {
  override val name: String               = "length"
  val minParameters: Int                  = 0
  val maxParameters: Int                  = 0
  val defaultParameters: Map[Int, Filter] = Map.empty
  override val isSingledValue: Boolean    = true
  override def invoke(input: Json, parameters: List[Filter], position: Int): Either[EvaluationException, List[Json]] = {
    input.fold(
      Right(List(Json.fromInt(0))),
      _ => Left(EvaluationException(position, "Boolean type has no length")),
      n => Right(List(Json.fromJsonNumber(n))),
      str => Right(List(Json.fromInt(str.size))),
      array => Right(List(Json.fromInt(array.size))),
      obj => Right(List(Json.fromInt(obj.size)))
    )
  }
}
