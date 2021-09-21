package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.{
  Filter,
  JsonObjectTemplate,
  JsonObjectTemplateConstructor,
  JsonTupleHeader,
  Selector
}
import org.fluminous.jq.tokens.{
  Identifier,
  LeftFigureBracket,
  RawString,
  RightFigureBracket
}
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, check }
import shapeless.HNil
import shapeless.::

case object JsonObjectTemplatePattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObjectTemplate](
    (check[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectTemplateConstructor
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObjectTemplate(_, js.values + (jh.fieldName -> s))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObjectTemplate(_, js.values + (id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectTemplateConstructor]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObjectTemplate(_, js.values + (s.value -> Selector(s.position, Seq(s.value))))
    },
    (check[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObjectTemplate(_, Map(jh.fieldName -> s))
    },
    (check[RightFigureBracket] ->: capture[Identifier] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObjectTemplate(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (check[RightFigureBracket] ->: capture[RawString] ->: check[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObjectTemplate(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    }
  )
}
