package org.fluminous.jq.filter.algebra

import cats.data.NonEmptyList
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, captureIf, lookup, test, testThat }
import org.fluminous.jq.tokens.{ Identifier, LeftBracket, RightBracket }
import shapeless.{ ::, HNil }

case object AlgebraExpressionPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[AlgebraExpression](
    captureIf[Identifier](id => OperationSign.signs.contains(id.value)).ifValidReplaceBy {
      case id :: HNil => OperationSign(id.value)
    },
    (capture[Filter] ->: capture[OperationSign] ->: capture[Filter]).ifValidReplaceBy {
      case right :: operation :: left :: HNil => AlgebraExpressionStart(_, operation, left, right)
    },
    (capture[Filter] ->: capture[OperationSign] ->: capture[AlgebraExpressionStart]).ifValidReplaceBy {
      case filter :: operation :: expression :: HNil => _ => expression.addFilter(operation, filter)
    },
    (lookup[Expression].notInstance[OperationSign] ->: capture[AlgebraExpressionStart]).ifValidReplaceBy {
      case expression :: HNil => AlgebraExpression(_, expression.operationSign, expression.left, expression.right)
    },
    (test[RightBracket] ->: capture[Filter] ->: test[LeftBracket]).ifValidReplaceBy {
      case filter :: HNil => _ => filter
    },
    (capture[Filter] ->: testThat[Identifier](_.value == "-")).ifValidReplaceBy {
      case filter :: HNil => _ => filter
    }
  )
}
