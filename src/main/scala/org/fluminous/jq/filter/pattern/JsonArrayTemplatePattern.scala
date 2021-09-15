package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.{ DecimalNumber, IntegerNumber, LeftSquareBracket, RawString, RightSquareBracket, Root }

import org.fluminous.jq.filter.pattern.dsl.MatcherExpression.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonArrayTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: List[PatternCase] = List(
    (check[RightSquareBracket] ->: capture[Selector] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => List(JsonArrayTemplate((Right(s) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: check[Root] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case js :: HNil => List(JsonArrayTemplate((Right(Root) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => List(JsonArrayTemplate((Left(Json.fromString(s.value)) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case i :: js :: HNil => List(JsonArrayTemplate((Left(Json.fromInt(i.asInt)) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case n :: js :: HNil => List(JsonArrayTemplate((Left(Json.fromBigDecimal(n.asDecimal)) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => List(JsonArrayTemplate((Right(filter) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: capture[JsonArrayTemplateConstructor]).ifValidReplaceBy {
      case filter :: js :: HNil => List(JsonArrayTemplate((Right(filter) +: js.values).reverse))
    },
    (check[RightSquareBracket] ->: capture[Selector] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => List(JsonArrayTemplate(Seq(Right(s))))
    },
    (check[RightSquareBracket] ->: check[Root] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case HNil => List(JsonArrayTemplate(Seq(Right(Root))))
    },
    (check[RightSquareBracket] ->: capture[RawString] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case s :: HNil => List(JsonArrayTemplate(Seq(Left(Json.fromString(s.value)))))
    },
    (check[RightSquareBracket] ->: capture[IntegerNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case i :: HNil => List(JsonArrayTemplate(Seq(Left(Json.fromInt(i.asInt)))))
    },
    (check[RightSquareBracket] ->: capture[DecimalNumber] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case d :: HNil => List(JsonArrayTemplate(Seq(Left(Json.fromBigDecimal(d.asDecimal)))))
    },
    (check[RightSquareBracket] ->: capture[JsonObjectTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => List(JsonArrayTemplate(Seq(Right(filter))))
    },
    (check[RightSquareBracket] ->: capture[JsonArrayTemplate] ->: check[LeftSquareBracket]).ifValidReplaceBy {
      case filter :: HNil => List(JsonArrayTemplate(Seq(Right(filter))))
    }
  )
}
