package org.fluminous.jq.filter.range

import org.fluminous.jq.filter.algebra.IntegerNumber
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test }
import org.fluminous.jq.tokens.NaturalNumber
import org.fluminous.jq.tokens.symbolic.{ Colon, LeftSquareBracket, RightSquareBracket }
import shapeless.{ ::, HNil }

class RangePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Range](
    (test[RightSquareBracket] ->: capture[IntegerNumber] ->: test[Colon] ->: capture[IntegerNumber] ->: test[
      LeftSquareBracket
    ]).ifValidReplaceBy {
      case right :: left :: HNil => Range(_, Some(left.value), Some(right.value))
    },
    (test[RightSquareBracket] ->: capture[IntegerNumber] ->: test[Colon] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case right :: HNil => Range(_, None, Some(right.value))
    },
    (test[RightSquareBracket] ->: test[Colon] ->: capture[IntegerNumber] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case left :: HNil => Range(_, Some(left.value), None)
    },
    (test[RightSquareBracket] ->: capture[NaturalNumber] ->: test[Colon] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case right :: HNil => Range(_, None, Some(right.intValue))
    },
    (test[RightSquareBracket] ->: test[Colon] ->: capture[NaturalNumber] ->: test[LeftSquareBracket]).ifValidReplaceBy {
      case left :: HNil => Range(_, Some(left.intValue), None)
    }
  )
}
