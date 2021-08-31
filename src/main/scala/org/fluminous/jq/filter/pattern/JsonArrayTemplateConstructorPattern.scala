package org.fluminous.jq.filter.pattern

import io.circe.Json
import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplateConstructor, Selector }
import org.fluminous.jq.tokens.{ Comma, DecimalNumber, IntegerNumber, LeftSquareBracket, RawString }

class JsonArrayTemplateConstructorPattern extends FilterPattern {
  override val FilterCases: PartialFunction[List[Expression], List[Expression]] = {
    case Comma :: (s @ Selector(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Right(s))) :: rest
    case Comma :: RawString(value, _) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromString(value)))) :: rest
    case Comma :: (value @ IntegerNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromInt(value.asInt)))) :: rest
    case Comma :: (value @ DecimalNumber(_)) :: LeftSquareBracket :: rest =>
      JsonArrayTemplateConstructor(Seq(Left(Json.fromBigDecimal(value.asDecimal)))) :: rest
    case (s @ Selector(_)) :: Comma :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Right(s) +: seq) :: rest
    case RawString(value, _) :: Comma :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromString(value)) +: seq) :: rest
    case (value @ IntegerNumber(_)) :: Comma :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromInt(value.asInt)) +: seq) :: rest
    case (value @ DecimalNumber(_)) :: Comma :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplateConstructor(Left(Json.fromBigDecimal(value.asDecimal)) +: seq) :: rest

  }
}
