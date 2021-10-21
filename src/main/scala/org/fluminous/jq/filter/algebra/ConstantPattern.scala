package org.fluminous.jq.filter.algebra

import org.fluminous.jq.filter.algebra.bool.BooleanConstant
import org.fluminous.jq.filter.pattern.dsl.Matcher.testThat
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.Identifier
import shapeless.HNil

object ConstantPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[BooleanConstant](
    testThat[Identifier](_.value.toLowerCase == "null").ifValidReplaceBy {
      case HNil => NullConstant(_)
    },
    testThat[Identifier](_.value.toLowerCase == "true").ifValidReplaceBy {
      case HNil => BooleanConstant(_, true)
    },
    testThat[Identifier](_.value.toLowerCase == "false").ifValidReplaceBy {
      case HNil => BooleanConstant(_, false)
    }
  )
}
