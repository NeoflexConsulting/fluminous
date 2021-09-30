package org.fluminous.jq.filter.pipe

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.selector.{ IdentitySelector, Selector }
import org.fluminous.jq.tokens.{ Identifier, RawString, Root }
import shapeless.{ ::, HNil }

class PipePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[FilterPipe](
    (capture[Identifier] ->: test[Root]).ifValidReplaceBy {
      case id :: HNil => FilterPipeStart(_, Seq(Selector(id.position, id.value)))
    },
    (capture[RawString] ->: test[Root]).ifValidReplaceBy {
      case s :: HNil => FilterPipeStart(_, Seq(Selector(s.position, s.value)))
    },
    (lookup[Expression] ->: capture[Root]).ifValidReplaceBy {
      case r :: HNil => FilterPipeStart(_, Seq(IdentitySelector(r.position)))
    },
    (capture[Identifier] ->: test[Root] ->: capture[FilterPipeStart]).ifValidReplaceBy {
      case id :: pipe :: HNil => FilterPipeStart(_, Selector(id.position, id.value) +: pipe.filters)
    },
    (capture[RawString] ->: test[Root] ->: capture[FilterPipeStart]).ifValidReplaceBy {
      case str :: pipe :: HNil => FilterPipeStart(_, Selector(str.position, str.value) +: pipe.filters)
    },
    (lookup[Expression] ->: capture[Root] ->: capture[FilterPipeStart]).ifValidReplaceBy {
      case r :: pipe :: HNil => FilterPipeStart(_, IdentitySelector(r.position) +: pipe.filters)
    }
    /*,
    (capture[Selector] ->: capture[Selector]).ifValidReplaceBy {
      case s1 :: s2 :: HNil => Selector(_, s2.path ++ s1.path)
    },
    (capture[Selector] ->: testAndDrop[Pipe] ->: capture[Selector]).ifValidReplaceBy {
      case s1 :: s2 :: HNil => Selector(_, s2.path ++ s1.path)
    }*/
  )

}
