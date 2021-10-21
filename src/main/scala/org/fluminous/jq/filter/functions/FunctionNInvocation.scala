package org.fluminous.jq.filter.functions

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.functions.definition.JqFunction

case class FunctionNInvocation(override val position: Int, name: String, parameters: List[Filter]) extends Filter {
  override val isSingleValued: Boolean = JqFunction.nNaryFunctions(name).isSingledValue
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    val function = JqFunction.nNaryFunctions(name)
    if (parameters.length < function.minParameters) {
      Left(
        EvaluationException(
          position,
          s"""Not enough parameters for function ${function.name}. Expected ${function.minParameters}, but found ${parameters.length}"""
        )
      )
    } else if (parameters.length > function.maxParameters) {
      Left(
        EvaluationException(
          position,
          s"""Too many parameters for function ${function.name}. Expected ${function.maxParameters}, but found ${parameters.length}"""
        )
      )
    } else {
      function.invoke(input, parameters, position)
    }
  }

  override val description: String = FunctionNInvocation.typeDescription.description
}

object FunctionNInvocation {
  implicit def typeDescription: Description[FunctionNInvocation] = new Description[FunctionNInvocation] {
    override val description: String = "function"
  }
}
