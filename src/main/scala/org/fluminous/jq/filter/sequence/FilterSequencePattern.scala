package org.fluminous.jq.filter.sequence

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.Comma
import shapeless.{ ::, HNil }

case object FilterSequencePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[FilterSequence](
    (test[Comma] ->: capture[Filter] ->: capture[FilterSequenceStart]).ifValidReplaceBy {
      case filter :: pipe :: HNil => FilterSequenceStart(_, filter +: pipe.filters)
    },
    (lookup[Expression]
      .notInstance[Comma] ->: capture[Filter] ->: capture[FilterSequenceStart]).ifValidReplaceBy {
      case filter :: pipe :: HNil => FilterSequence(_, (filter +: pipe.filters).reverse)
    },
    (test[Comma] ->: capture[Filter]).ifValidReplaceBy {
      case filter :: HNil => FilterSequenceStart(_, List(filter))
    }
  )
}
