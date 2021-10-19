package org.fluminous.jq.filter.algebra

import org.fluminous.jq.filter.pattern.{ExpressionPattern, PatternCases}
import org.fluminous.jq.filter.pattern.dsl.Matcher.{capture, test}
import org.fluminous.jq.tokens.NaturalNumber
import org.fluminous.jq.tokens.symbolic.Minus
import shapeless.{::, HNil}

object IntegerNumberPattern extends ExpressionPattern{
  override val ExpressionCases: PatternCases = PatternCases[AlgebraExpression](
    (capture[NaturalNumber] ->: test[Minus]).ifValidReplaceBy {
      case n :: HNil => IntegerNumber(_, -n.intValue)
    },
    capture[NaturalNumber].ifValidReplaceBy {
      case n :: HNil => IntegerNumber(_, n.intValue)
    }
  )
}
