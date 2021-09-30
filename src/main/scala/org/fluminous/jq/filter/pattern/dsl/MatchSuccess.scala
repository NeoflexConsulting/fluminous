package org.fluminous.jq.filter.pattern.dsl

import org.fluminous.jq.{Expression, Tokenizer}
import shapeless.HList

case class MatchSuccess[Captured <: HList](
  patternStartPosition: Int,
  bottomStack: List[Expression],
  capturedVariables: Captured)
