package org.fluminous.jq

import org.fluminous.jq.filter.pattern.PatternFailure

case class ParserFailure(failure: PatternFailure, firstFailurePosition: Int) {
  def mergeWith(newFailure: PatternFailure): Option[ParserFailure] = {
    if (firstFailurePosition >= newFailure.startPosition && newFailure.failurePosition > failure.failurePosition) {
      Some(this.copy(failure = newFailure))
    } else {
      None
    }
  }
}

object ParserFailure {
  def apply(failure: PatternFailure): ParserFailure = {
    ParserFailure(failure, failure.failurePosition)
  }
}

case class ParserState(stack: List[Expression] = List.empty, failInfo: Option[ParserFailure] = None) {
  def tokenSucceed(updatedStack: List[Expression]): ParserState = {
    ParserState(updatedStack, None)
  }

  def tokenFailed(patternFailure: Option[PatternFailure]): ParserState = {
    patternFailure.map(tokenFailed).getOrElse(this)
  }

  def tokenFailed(patternFailure: PatternFailure): ParserState = {
    this.failInfo
      .fold(this.copy(failInfo = Some(ParserFailure(patternFailure)))) { failInfo =>
        failInfo.mergeWith(patternFailure).fold(this)(pf => this.copy(failInfo = Some(pf)))
      }
  }
}
