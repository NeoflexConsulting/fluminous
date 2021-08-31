package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression

trait FilterPattern {
  def instantiateOnStack(stack: List[Expression]): Option[List[Expression]] = FilterCases.lift(stack)
  protected val FilterCases: PartialFunction[List[Expression], List[Expression]]
}

object FilterPattern {
  val patterns = List(SelectorPattern)
}
