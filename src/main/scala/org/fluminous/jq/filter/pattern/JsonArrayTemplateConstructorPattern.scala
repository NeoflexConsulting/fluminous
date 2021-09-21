package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, IntegerNumber, LeftSquareBracket, RawString, Root }
import shapeless.{ ::, HNil }

case object JsonArrayTemplateConstructorPattern extends ExpressionPattern {

  override val ExpressionCases: PatternCases = PatternCases[JsonArrayTemplateConstructor](
    (check[Comma] ->: capture[Selector] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplateConstructor(_, Seq(s))
    },
    (check[Comma] ->: capture[Root] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case r :: HNil => JsonArrayTemplateConstructor(_, Seq(r))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplateConstructor(_, Seq(s))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case i :: HNil => JsonArrayTemplateConstructor(_, Seq(i))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case n :: HNil => JsonArrayTemplateConstructor(_, Seq(n))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplateConstructor(_, Seq(filter))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplateConstructor(_, Seq(filter))
    },
    (check[Comma] ->: capture[Selector] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplateConstructor(_, s +: js.values)
    },
    (check[Comma] ->: capture[Root] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case r :: js :: HNil => JsonArrayTemplateConstructor(_, r +: js.values)
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplateConstructor(_, s +: js.values)
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case i :: js :: HNil => JsonArrayTemplateConstructor(_, i +: js.values)
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case n :: js :: HNil => JsonArrayTemplateConstructor(_, n +: js.values)
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplateConstructor(_, filter +: js.values)
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplateConstructor(_, filter +: js.values)
    }
  )
}
