package org.fluminous.jq

import org.fluminous.jq.filter.pattern.dsl.{ InconsistencyMeasure, MatcherReport }

import scala.math.Ordering.Implicits.infixOrderingOps

case class FailInfo(
  matcherInfo: MatcherReport,
  inconsistencyMeasure: InconsistencyMeasure,
  position: Int,
  nextActualToken: Option[Expression])

case class ParserState(stack: List[Expression] = List.empty, failInfo: Option[FailInfo] = None) {
  def resetFailInfo: ParserState = this.copy(failInfo = None)
  def saveFailedMatcher(matcherInfo: MatcherReport, position: Int, i: InconsistencyMeasure): ParserState = {
    this.failInfo
      .filter(_.inconsistencyMeasure <= i)
      .fold(this.copy(failInfo = Some(FailInfo(matcherInfo, i, position, None))))(_ => this)
  }
  def saveActualToken(token: Expression): ParserState = {
    this.failInfo
      .map(f => f.nextActualToken.fold(this.copy(failInfo = Some(f.copy(nextActualToken = Some(token)))))(_ => this))
      .getOrElse(this)
  }
}
