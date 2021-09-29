package org.fluminous.jq.filter.boolean

import org.fluminous.jq.{ Description, Expression }
import org.fluminous.jq.filter.Filter

final case class ConjunctionUncompleted(override val position: Int, values: List[Filter]) extends Expression {
  override val description: String = ConjunctionUncompleted.typeDescription.description
}

object ConjunctionUncompleted {
  implicit def typeDescription: Description[ConjunctionUncompleted] = new Description[ConjunctionUncompleted] {
    override val description: String = "conjunction"
  }
}
