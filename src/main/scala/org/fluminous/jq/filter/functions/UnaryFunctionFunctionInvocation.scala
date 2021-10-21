package org.fluminous.jq.filter.functions

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.functions.definition.JqFunction

case class UnaryFunctionFunctionInvocation(override val position: Int, name: String) extends Filter {
  override val isSingleValued: Boolean = JqFunction.unaryFunctions(name).isSingledValue
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    JqFunction.unaryFunctions(name).invoke(input, List.empty, position)
  }
  override val description: String = UnaryFunctionFunctionInvocation.typeDescription.description
}

object UnaryFunctionFunctionInvocation {
  implicit def typeDescription: Description[UnaryFunctionFunctionInvocation] =
    new Description[UnaryFunctionFunctionInvocation] {
      override val description: String = "function"
    }
}
