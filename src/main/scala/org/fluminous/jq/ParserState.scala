package org.fluminous.jq

import org.fluminous.jq.filter.pattern.PatternFailure
import cats.syntax.foldable._

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
  def tokenFailed(patternFailures: List[PatternFailure]): ParserState = {
    getMostSuitable(patternFailures, this.failInfo).fold(this)(tokenFailed)
  }

  private def tokenFailed(patternFailure: PatternFailure): ParserState = {
    this.failInfo
      .fold(this.copy(failInfo = Some(ParserFailure(patternFailure)))) { failInfo =>
        failInfo.mergeWith(patternFailure).fold(this)(pf => this.copy(failInfo = Some(pf)))
      }
  }

  private def getMostSuitable(
    patternFailures: List[PatternFailure],
    failInfo: Option[ParserFailure]
  ): Option[PatternFailure] = {
    failInfo.fold(getMostSuitable(patternFailures))(getMostSuitable(patternFailures, _))
  }

  private def getMostSuitable(patternFailures: List[PatternFailure]): Option[PatternFailure] = {
    patternFailures.maximumByOption(f => (f.failurePosition, -f.mismatchQty))
  }

  private def getMostSuitable(
    patternFailures: List[PatternFailure],
    failInfo: ParserFailure
  ): Option[PatternFailure] = {
    patternFailures
      .filter(_.startPosition <= failInfo.firstFailurePosition)
      .maximumByOption(f => (f.failurePosition, -f.mismatchQty))
  }
}
