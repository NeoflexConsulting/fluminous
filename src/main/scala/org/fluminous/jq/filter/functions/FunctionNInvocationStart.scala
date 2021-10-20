package org.fluminous.jq.filter.functions

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, Expression }

final case class FunctionNInvocationStart(override val position: Int, name: String, parameters: List[Filter]) extends Expression {
  override val description: String = FunctionNInvocationStart.typeDescription.description
}

object FunctionNInvocationStart {
  implicit def typeDescription: Description[FunctionNInvocationStart] =
    new Description[FunctionNInvocationStart] {
      override val description: String = "function"
    }
}
