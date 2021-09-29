package org.fluminous.jq.filter.pattern

import org.fluminous.jq.filter.json.{ JsonObject, JsonObjectUncompleted, JsonTupleHeader }
import org.fluminous.jq.filter.{ Filter, Selector }
import org.fluminous.jq.tokens.{ Identifier, LeftFigureBracket, RawString, RightFigureBracket }
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, testAndDrop }
import shapeless.HNil
import shapeless.::

case object JsonObjectPattern extends ExpressionPattern {
  override val ExpressionCases: PatternCases = PatternCases[JsonObject](
    (testAndDrop[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: capture[
      JsonObjectUncompleted
    ]).ifValidReplaceBy {
      case s :: jh :: js :: HNil => JsonObject(_, js.values + (jh.fieldName -> s))
    },
    (testAndDrop[RightFigureBracket] ->: capture[Identifier] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case id :: js :: HNil =>
        JsonObject(_, js.values + (id.value -> Selector(id.position, Seq(id.value))))
    },
    (testAndDrop[RightFigureBracket] ->: capture[RawString] ->: capture[JsonObjectUncompleted]).ifValidReplaceBy {
      case s :: js :: HNil => JsonObject(_, js.values + (s.value -> Selector(s.position, Seq(s.value))))
    },
    (testAndDrop[RightFigureBracket] ->: capture[Filter] ->: capture[JsonTupleHeader] ->: testAndDrop[
      LeftFigureBracket
    ]).ifValidReplaceBy {
      case s :: jh :: HNil => JsonObject(_, Map(jh.fieldName -> s))
    },
    (testAndDrop[RightFigureBracket] ->: capture[Identifier] ->: testAndDrop[LeftFigureBracket]).ifValidReplaceBy {
      case id :: HNil => JsonObject(_, Map(id.value -> Selector(id.position, Seq(id.value))))
    },
    (testAndDrop[RightFigureBracket] ->: capture[RawString] ->: testAndDrop[LeftFigureBracket]).ifValidReplaceBy {
      case s :: HNil => JsonObject(_, Map(s.value -> Selector(s.position, Seq(s.value))))
    }
  )
}
