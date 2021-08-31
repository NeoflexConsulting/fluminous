package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonObjectTemplate, JsonObjectTemplateConstructor, JsonTemplateTuple, Selector }
import org.fluminous.jq.tokens.{ Identifier, LeftFigureBracket, RawString, RightFigureBracket }

case object JsonObjectTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case RightFigureBracket :: JsonTemplateTuple(name, value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (name -> value)) :: rest
    case RightFigureBracket :: Identifier(value) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (value -> Right(Selector(Seq(value))))) :: rest

    case RightFigureBracket :: RawString(value, _) :: JsonObjectTemplateConstructor(values) :: rest =>
      JsonObjectTemplate(values + (value -> Right(Selector(Seq(value))))) :: rest

    case RightFigureBracket :: JsonTemplateTuple(name, value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(name -> value)) :: rest

    case RightFigureBracket :: Identifier(value) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(value -> Right(Selector(Seq(value))))) :: rest

    case RightFigureBracket :: RawString(value, _) :: LeftFigureBracket :: rest =>
      JsonObjectTemplate(Map(value -> Right(Selector(Seq(value))))) :: rest

  }
}
