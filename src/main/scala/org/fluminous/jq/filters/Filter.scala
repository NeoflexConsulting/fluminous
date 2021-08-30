package org.fluminous.jq.filters

import org.fluminous.jq.Expression

trait Filter extends Expression {
  def isSuitableForStack(stack: List[Expression]): Boolean          = FilterStartCases(stack)
  def instantiateOnStack(stack: List[Expression]): List[Expression] = FilterCases.lift(stack).getOrElse(stack)
  protected val FilterStartCases: List[Expression] => Boolean
  protected val FilterCases: PartialFunction[List[Expression], List[Expression]]
}
