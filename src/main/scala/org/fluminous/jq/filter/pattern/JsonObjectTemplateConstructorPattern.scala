package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonObjectTemplateConstructor, JsonTemplateTuple, Selector }
import org.fluminous.jq.tokens.{ Comma, Identifier, LeftFigureBracket, RawString }

case object JsonObjectTemplateConstructorPattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case Comma :: JsonTemplateTuple(name, value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(name -> value)) :: rest
    case Comma :: Identifier(value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(value -> Right(Selector(Seq(value))))) :: rest
    case Comma :: RawString(value, _) :: LeftFigureBracket :: rest =>
      JsonObjectTemplateConstructor(Map(value -> Right(Selector(Seq(value))))) :: rest

    case Comma :: JsonTemplateTuple(name, value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (name -> value)) :: rest
    case Comma :: Identifier(value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (value -> Right(Selector(Seq(value))))) :: rest
    case Comma :: RawString(value, _) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplateConstructor(values + (value -> Right(Selector(Seq(value))))) :: rest
  }
}
