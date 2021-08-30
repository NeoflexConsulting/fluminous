package org.fluminous.jq.filters

import org.fluminous.jq.Expression
import org.fluminous.jq.tokens.{ Identifier, RawString, Root }

final case class Selector(path: Seq[String]) extends Filter {

  override val FilterStartCases: List[Expression] => Boolean = {
    case Nil              => true
    case Root :: _        => true
    case Selector(_) :: _ => true
    case _                => false
  }

  override val FilterCases: PartialFunction[List[Expression], List[Expression]] = {
    case Identifier(value) :: Root :: rest                   => Selector(Seq(value)) :: rest
    case RawString(value, _) :: Root :: rest                 => Selector(Seq(value)) :: rest
    case Selector(pathChild) :: Selector(pathParent) :: rest => Selector(pathParent ++ pathChild) :: rest
  }
}
