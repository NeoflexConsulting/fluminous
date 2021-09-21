package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ DecimalNumber, IntegerNumber, LeftSquareBracket, RawString, RightSquareBracket, Root }

import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonArrayTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonArrayTemplate](
    (check[RightSquareBracket] ->: capture[Selector] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplate(_, (s +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[Root] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case r :: js :: HNil => JsonArrayTemplate(_, (r +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplate(_, (s +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case i :: js :: HNil => JsonArrayTemplate(_, (i +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case n :: js :: HNil => JsonArrayTemplate(_, (n +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplate(_, (filter +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplate(_, (filter +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[Selector] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplate(_, Seq(s))
    },
    (check[RightSquareBracket] ->: capture[Root] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case r :: HNil => JsonArrayTemplate(_, Seq(r))
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplate(_, Seq(s))
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case i :: HNil => JsonArrayTemplate(_, Seq(i))
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case d :: HNil => JsonArrayTemplate(_, Seq(d))
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplate(_, Seq(filter))
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplate(_, Seq(filter))
    }
  )
}
