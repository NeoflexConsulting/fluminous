package org.fluminous.jq

import org.fluminous.jq.filter.pattern.PatternFailure
import cats.syntax.foldable._

case class ParserFailure(failures: Seq[PatternFailure], position: Int) {
  def mergeWith(parserFailure: ParserFailure): Option[ParserFailure] = {
    if (position == parserFailure.position) {
      Some(ParserFailure(failures ++ parserFailure.failures, position))
    } else {
      None
    }
  }
  def formatError: String = {
    val failuresText = failures.map(f =>
      s" If was supposed ${f.patternName} then expected ${formatExpected(f)}, but found: ${formatActual(f)}."
    )
    s"Parsing error at position $position.$failuresText"
  }

  private def formatActual(f: PatternFailure): String = {
    f.failures.headOption.map(_.actualExpression).getOrElse("")
  }

  private def formatExpected(f: PatternFailure): String = {
    if (f.failures.size == 1)
      f.failures.head.expectedExpression
    else
      s"either ${f.failures.map(_.expectedExpression).mkString(" or ")}"
  }

}

object ParserFailure {
  def apply(failures: List[PatternFailure]): ParserFailure = {
    val nearestFailures = failures.minimumByList(_.position)
    ParserFailure(nearestFailures, nearestFailures.map(_.position).min)
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
