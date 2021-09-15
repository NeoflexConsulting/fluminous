package org.fluminous.jq.filter.pattern

import cats.data.NonEmptyList
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.InconsistencyMeasure
import cats.syntax.foldable._

trait ExpressionPattern {
  type PatternCase = NonEmptyList[Expression] => Either[InconsistencyMeasure, List[Expression]]
  def instantiateOnStack(stack: NonEmptyList[Expression]): Option[List[Expression]] =
    ExpressionCases.foldMapK(f => f(stack).toOption)

  protected val ExpressionCases: List[PatternCase]
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
