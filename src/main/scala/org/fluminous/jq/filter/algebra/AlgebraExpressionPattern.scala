package org.fluminous.jq.filter.algebra

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, captureIf, lookup, test }
import org.fluminous.jq.tokens.Identifier
import org.fluminous.jq.tokens.symbolic.{ LeftBracket, Minus, RightBracket }
import shapeless.{ ::, HNil }

case object AlgebraExpressionPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[AlgebraExpression](
    captureIf[Identifier](id => OperationalIdentifier.identifiers.contains(id.value)).ifValidReplaceBy {
      case id :: HNil => OperationalIdentifier(id.value)
    },
    (capture[Filter] ->: capture[AlgebraOperation] ->: capture[Filter]).ifValidReplaceBy {
      case right :: operation :: left :: HNil => AlgebraExpressionStart(_, operation, left, right)
    },
    (capture[Filter] ->: capture[AlgebraOperation] ->: capture[AlgebraExpressionStart]).ifValidReplaceBy {
      case filter :: operation :: expression :: HNil => _ => expression.addFilter(operation, filter)
    },
    (lookup[Expression].notInstance[AlgebraOperation] ->: capture[AlgebraExpressionStart]).ifValidReplaceBy {
      case expression :: HNil => AlgebraExpression(_, expression.operationSign, expression.left, expression.right)
    },
    (test[RightBracket] ->: capture[Filter] ->: test[LeftBracket]).ifValidReplaceBy {
      case filter :: HNil => _ => filter
    },
    (capture[Filter] ->: test[Minus]).ifValidReplaceBy {
      case filter :: HNil => NegatedFilter(_, filter)
    }
  )
}
