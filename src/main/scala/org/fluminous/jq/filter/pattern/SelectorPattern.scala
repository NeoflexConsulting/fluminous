package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Selector
import org.fluminous.jq.tokens.{ Identifier, Pipe, RawString, Root }

case object SelectorPattern extends ExpressionPattern {
  override val ExpressionCases: PartialFunction[List[Expression], List[Expression]] = {
    case Identifier(value) :: Root :: rest =>
      Selector(Seq(value)) :: rest
    case RawString(value, _) :: Root :: rest =>
      Selector(Seq(value)) :: rest
    case Selector(pathChild) :: Selector(pathParent) :: rest =>
      Selector(pathParent ++ pathChild) :: rest
    case Selector(pathChild) :: Pipe :: Selector(pathParent) :: rest =>
      Selector(pathParent ++ pathChild) :: rest
  }
}
