package org.fluminous.jq.filter.pipe

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.symbolic.{ QuestionMark, Root, VerticalSlash }
import shapeless.{ ::, HNil }
case object PipePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[Pipe](
    (test[VerticalSlash] ->: capture[Filter] ->: capture[PipeStart]).ifValidReplaceBy {
      case filter :: pipe :: HNil => PipeStart(_, filter +: pipe.filters)
    },
    (lookup[Root] ->: capture[Filter] ->: capture[PipeStart]).ifValidReplaceBy {
      case filter :: pipe :: HNil => PipeStart(_, filter +: pipe.filters)
    },
    (lookup[Expression]
      .notInstance[VerticalSlash]
      .notInstance[Root]
      .notInstance[QuestionMark] ->: capture[Filter] ->: capture[PipeStart]).ifValidReplaceBy {
      case filter :: pipe :: HNil => Pipe(_, (filter +: pipe.filters).reverse)
    },
    (test[VerticalSlash] ->: capture[Filter]).ifValidReplaceBy {
      case filter :: HNil => PipeStart(_, List(filter))
    },
    (lookup[Root] ->: capture[Filter]).ifValidReplaceBy {
      case filter :: HNil => PipeStart(_, List(filter))
    }
  )

}
