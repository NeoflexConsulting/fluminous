package org.fluminous.jq.filter.pattern

import cats.data.NonEmptyList
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.InconsistencyMeasure
import cats.syntax.foldable._

trait ExpressionPattern {
  def instantiateOnStack(stack: NonEmptyList[Expression]): Option[List[Expression]] =
    ExpressionCases.cases.foldMapK(f => f(stack).toOption)

  protected val ExpressionCases: PatternCases
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
