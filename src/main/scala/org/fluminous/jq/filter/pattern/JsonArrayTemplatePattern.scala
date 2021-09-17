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
      case s :: js :: HNil => JsonArrayTemplate(_, (Right(s) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[Root] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case r :: js :: HNil => JsonArrayTemplate(_, (Right(r) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonArrayTemplate(_, (Left(Json.fromString(s.value)) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case i :: js :: HNil => JsonArrayTemplate(_, (Left(Json.fromInt(i.asInt)) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case n :: js :: HNil => JsonArrayTemplate(_, (Left(Json.fromBigDecimal(n.asDecimal)) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplate(_, (Right(filter) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => JsonArrayTemplate(_, (Right(filter) +: js.values).reverse)
    },
    (check[RightSquareBracket] ->: capture[Selector] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplate(_, Seq(Right(s)))
    },
    (check[RightSquareBracket] ->: capture[Root] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case r :: HNil => JsonArrayTemplate(_, Seq(Right(r)))
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => JsonArrayTemplate(_, Seq(Left(Json.fromString(s.value))))
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case i :: HNil => JsonArrayTemplate(_, Seq(Left(Json.fromInt(i.asInt))))
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case d :: HNil => JsonArrayTemplate(_, Seq(Left(Json.fromBigDecimal(d.asDecimal))))
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplate(_, Seq(Right(filter)))
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => JsonArrayTemplate(_, Seq(Right(filter)))
    }
  )
}
