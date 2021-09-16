package org.fluminous.jq

import org.fluminous.jq.filter.pattern.PatternFailure
import org.fluminous.jq.filter.pattern.dsl.MismatchesQty

case class ParserFailure(failures: Seq[PatternFailure], positionInStack: Int)

case class ParserState(stack: List[Expression] = List.empty, failInfo: Option[ParserFailure] = None) {
  def resetFailInfo: ParserState = this.copy(failInfo = None)
  def saveFailedMatcher(matcherInfo: MatcherReport, position: Int, i: MismatchesQty): ParserState = {
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
