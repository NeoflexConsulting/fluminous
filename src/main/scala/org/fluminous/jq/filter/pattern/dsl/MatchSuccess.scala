package org.fluminous.jq.filter.pattern.dsl

import org.fluminous.jq.Expression
import shapeless.HList

case class MatchSuccess[Captured <: HList](
  patternStartPosition: Int,
  stackTail: List[Expression],
  capturedVariables: Captured)
