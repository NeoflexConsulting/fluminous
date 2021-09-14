package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression

trait ExpressionPattern {
  def instantiateOnStack(stack: List[Expression]): Option[List[Expression]] = ExpressionCases.lift(stack)
  protected val ExpressionCases: Seq[PatternCase]
}

object ExpressionPattern {
  val patterns = List(
    SelectorPattern,
    JsonTupleHeaderPattern,
    JsonObjectTemplatePattern,
    JsonObjectTemplateConstructorPattern,
    JsonArrayTemplatePattern,
    JsonArrayTemplateConstructorPattern
  )
}
