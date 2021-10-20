package org.fluminous.jq.filter.functions.definition

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.Filter

trait JqFunction {
  val name: String
  val minParameters: Int
  val maxParameters: Int
  val defaultParameters: Map[Int, Filter]
  val isSingledValue: Boolean
  def invoke(input: Json, parameters: List[Filter], position: Int): Either[EvaluationException, List[Json]]
}

object JqFunction {
  private val allFunctions: List[JqFunction] = List(Length, Not, Recurse)

  val unaryFunctions: Map[String, JqFunction] =
    allFunctions.filter(_.minParameters == 0).map(f => (f.name, f)).toMap

  val nNaryFunctions: Map[String, JqFunction] =
    allFunctions.filter(_.maxParameters > 0).map(f => (f.name, f)).toMap
}
