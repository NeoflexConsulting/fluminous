package org.fluminous.jq.filters

import org.fluminous.jq.Expression

trait Filter extends Expression {
  def matches(stack: List[Expression]): Boolean                     = patterns.isDefinedAt(stack)
  def instantiateOnStack(stack: List[Expression]): List[Expression] = patterns.lift(stack).getOrElse(stack)
  protected val patterns: PartialFunction[List[Expression], List[Expression]]
}
