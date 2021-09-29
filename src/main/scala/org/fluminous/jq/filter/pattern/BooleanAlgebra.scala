package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.boolean.{ BooleanConstant, Conjunction, ConjunctionUncompleted }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, test, testAndDropIf }
import org.fluminous.jq.tokens.Identifier
import shapeless.{ ::, HNil }

case object BooleanAlgebra extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Conjunction](
    testAndDropIf[Identifier](_.value == "true").ifValidReplaceBy {
      case HNil => BooleanConstant(_, true)
    },
    testAndDropIf[Identifier](_.value == "false").ifValidReplaceBy {
      case HNil => BooleanConstant(_, false)
    },
    (capture[Filter] ->: testAndDropIf[Identifier](_.value == "and") ->: capture[Filter]).ifValidReplaceBy {
      case right :: left :: HNil => ConjunctionUncompleted(_, List(right, left))
    },
    (capture[Filter] ->: testAndDropIf[Identifier](_.value == "and") ->: capture[ConjunctionUncompleted]).ifValidReplaceBy {
      case right :: left :: HNil => ConjunctionUncompleted(_, right +: left.values)
    },
    (test[Expression] ->: capture[ConjunctionUncompleted]).ifValidReplaceBy {
      case conj :: HNil => Conjunction(_, conj.values.reverse)
    }
  )
}
