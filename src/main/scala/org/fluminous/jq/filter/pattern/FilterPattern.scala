package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression

trait FilterPattern {
  def isSuitableForStack(stack: List[Expression]): Boolean          = FilterStartCases(stack)
  def instantiateOnStack(stack: List[Expression]): List[Expression] = FilterCases.lift(stack).getOrElse(stack)
  protected val FilterStartCases: List[Expression] => Boolean
  protected val FilterCases: PartialFunction[List[Expression], List[Expression]]
}

object FilterPattern {
  val patterns = Seq(SelectorPattern)
}
