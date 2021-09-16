package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Selector
import org.fluminous.jq.filter.pattern.dsl.MatcherExpression.{ capture, check }
import org.fluminous.jq.tokens.{ Identifier, Pipe, RawString, Root }
import shapeless.HNil
import shapeless.::

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: List[PatternCase] = List(
    (capture[Identifier] ->: check[Root]).ifValidReplaceBy {
      case id :: HNil => List(Selector(Seq(id.value)))
    },
    (capture[RawString] ->: check[Root]).ifValidReplaceBy {
      case s :: HNil => List(Selector(Seq(s.value)))
    },
    (capture[Selector] ->: capture[Selector]).ifValidReplaceBy {
      case s1 :: s2 :: HNil => List(Selector(s2.path ++ s1.path))
    },
    (capture[Selector] ->: check[Pipe] ->: capture[Selector]).ifValidReplaceBy {
      case s1 :: s2 :: HNil => List(Selector(s2.path ++ s1.path))
    }
  )
}
