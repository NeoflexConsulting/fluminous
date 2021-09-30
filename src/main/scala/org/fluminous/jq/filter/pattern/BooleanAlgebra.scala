package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.boolean.{ BooleanConstant, Conjunction, ConjunctionUncompleted }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, testThat }
import org.fluminous.jq.tokens.Identifier
import shapeless.{ ::, HNil }

case object BooleanAlgebra extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Conjunction](
    testThat[Identifier](_.value == "true").ifValidReplaceBy {
      case HNil => BooleanConstant(_, true)
    },
    testThat[Identifier](_.value == "false").ifValidReplaceBy {
      case HNil => BooleanConstant(_, false)
    },
    (testThat[Identifier](_.value == "and") ->: capture[Filter]).ifValidReplaceBy {
      case right :: HNil => ConjunctionUncompleted(_, List(right))
    },
    (testThat[Identifier](_.value == "and") ->: capture[Filter] ->: capture[ConjunctionUncompleted]).ifValidReplaceBy {
      case right :: left :: HNil => ConjunctionUncompleted(_, right +: left.values)
    },
    (lookup[Expression] ->: capture[Filter] ->: capture[ConjunctionUncompleted]).ifValidReplaceBy {
      case right :: conj :: HNil => Conjunction(_, (right +: conj.values).reverse)
    }
  )
}
