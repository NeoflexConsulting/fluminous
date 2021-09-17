package org.fluminous.jq

import org.fluminous.jq.filter.pattern.PatternFailure
import cats.syntax.foldable._

case class ParserFailure(failures: Seq[PatternFailure], positionInStack: Int) {
  def mergeWith(parserFailure: ParserFailure): Option[ParserFailure] = {
    if (positionInStack == parserFailure.positionInStack) {
      Some(ParserFailure(failures ++ parserFailure.failures, positionInStack))
    } else {
      None
    }
  }
}

object ParserFailure {
  def apply(failures: List[PatternFailure]): ParserFailure = {
    val nearestFailures = failures.minimumByList(_.firstMismatchPositionInStack)
    ParserFailure(nearestFailures, nearestFailures.map(_.firstMismatchPositionInStack).min)
  }
}

case class ParserState(stack: List[Expression] = List.empty, failInfo: Option[ParserFailure] = None) {
  def tokenSucceed(updatedStack: List[Expression]): ParserState = ParserState(updatedStack, None)
  def tokenFailed(parserFailure: ParserFailure): ParserState = {
    this.failInfo
      .fold(this.copy(failInfo = Some(parserFailure))) { failInfo =>
        failInfo.mergeWith(parserFailure).fold(this)(pf => this.copy(failInfo = Some(pf)))
      }
  }
}
