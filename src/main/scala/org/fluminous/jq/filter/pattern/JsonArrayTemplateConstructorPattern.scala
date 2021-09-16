package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, IntegerNumber, LeftSquareBracket, RawString, Root }
import shapeless.{ ::, HNil }

case object JsonArrayTemplateConstructorPattern extends ExpressionPattern {

  override val ExpressionCases: PatternCases = PatternCases[JsonArrayTemplateConstructor](
    (check[Comma] ->: capture[Selector] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => List(JsonArrayTemplateConstructor(Seq(Right(s))))
    },
    (check[Comma] ->: check[Root] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case HNil => List(JsonArrayTemplateConstructor(Seq(Right(Root))))
    },
    (check[Comma] ->: capture[RawString] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => List(JsonArrayTemplateConstructor(Seq(Left(Json.fromString(s.value)))))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case i :: HNil => List(JsonArrayTemplateConstructor(Seq(Left(Json.fromInt(i.asInt)))))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case n :: HNil => List(JsonArrayTemplateConstructor(Seq(Left(Json.fromBigDecimal(n.asDecimal)))))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => List(JsonArrayTemplateConstructor(Seq(Right(filter))))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => List(JsonArrayTemplateConstructor(Seq(Right(filter))))
    },
    (check[Comma] ->: capture[Selector] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => List(JsonArrayTemplateConstructor(Right(s) +: js.values))
    },
    (check[Comma] ->: check[Root] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case js :: HNil => List(JsonArrayTemplateConstructor(Right(Root) +: js.values))
    },
    (check[Comma] ->: capture[RawString] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => List(JsonArrayTemplateConstructor(Left(Json.fromString(s.value)) +: js.values))
    },
    (check[Comma] ->: capture[IntegerNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case i :: js :: HNil => List(JsonArrayTemplateConstructor(Left(Json.fromInt(i.asInt)) +: js.values))
    },
    (check[Comma] ->: capture[DecimalNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case n :: js :: HNil => List(JsonArrayTemplateConstructor(Left(Json.fromBigDecimal(n.asDecimal)) +: js.values))
    },
    (check[Comma] ->: capture[JsonObjectTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => List(JsonArrayTemplateConstructor(Right(filter) +: js.values))
    },
    (check[Comma] ->: capture[JsonArrayTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => List(JsonArrayTemplateConstructor(Right(filter) +: js.values))
    }
  )
}
