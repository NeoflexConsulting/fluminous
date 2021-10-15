package org.fluminous.jq.filter.range

import org.fluminous.jq.filter.algebra.IntegerNumber
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.tokens.NaturalNumber
import org.fluminous.jq.tokens.symbolic.{ Colon, LeftBracket, RightBracket }
import shapeless.{ ::, HNil }

class RangePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Range](
    (test[RightBracket] ->: capture[IntegerNumber] ->: test[Colon] ->: capture[IntegerNumber] ->: test[LeftBracket]).ifValidReplaceBy {
      case right :: left :: HNil => Range(_, Some(left.value), Some(right.value))
    },
    (test[RightBracket] ->: capture[IntegerNumber] ->: test[Colon] ->: test[LeftBracket]).ifValidReplaceBy {
      case right :: HNil => Range(_, None, Some(right.value))
    },
    (test[RightBracket] ->: test[Colon] ->: capture[IntegerNumber] ->: test[LeftBracket]).ifValidReplaceBy {
      case left :: HNil => Range(_, Some(left.value), None)
    },
    (test[RightBracket] ->: capture[NaturalNumber] ->: test[Colon] ->: test[LeftBracket]).ifValidReplaceBy {
      case right :: HNil => Range(_, None, Some(right.intValue))
    },
    (test[RightBracket] ->: test[Colon] ->: capture[NaturalNumber] ->: test[LeftBracket]).ifValidReplaceBy {
      case left :: HNil => Range(_, Some(left.intValue), None)
    }
  )
}
