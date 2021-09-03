package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.JsonTupleHeader
import org.fluminous.jq.tokens.{ Colon, Identifier, RawString }

case object JsonTupleHeaderPattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case Colon :: Identifier(name) :: rest =>
      JsonTupleHeader(name) :: rest
    case Colon :: RawString(name, _) :: rest =>
      JsonTupleHeader(name) :: rest
  }
}
