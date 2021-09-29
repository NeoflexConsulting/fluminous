package org.fluminous.jq

import cats.data.NonEmptyList
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.ExpressionPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.Token
import cats.syntax.foldable._
import scala.annotation.tailrec
import org.slf4j.LoggerFactory

trait Parser extends FoldFunctions {
  private val logger = LoggerFactory.getLogger(getClass)
  type Stack = List[org.fluminous.jq.Expression]
  def parse(input: InputProvider): Either[ParserException, Filter] = {
    parse(Tokenizer(input))
  }
  @tailrec
  private def parse(tokenizer: Tokenizer, state: ParserState = ParserState()): Either[ParserException, Filter] = {
    tokenizer.nextToken match {
      case Left(ex) =>
        Left(ex)
      case Right((updatedTokenizer, None)) =>
        getFilterFromStack(updatedTokenizer, foldStack(state.stack), state.failInfo)
      case Right((updatedTokenizer, Some(token))) =>
        parse(updatedTokenizer, applyTokenToStack(token, state))
    }
  }

  private def getFilterFromStack(
    tokenizer: Tokenizer,
    stack: Stack,
    failInfo: Option[ParserFailure]
  ): Either[ParserException, Filter] = {
    stack match {
      case Nil =>
        Left(ParserException(tokenizer.input.position, "Input is empty"))
      case (filter: Filter) :: Nil =>
        Right(filter)
      case expr :: Nil =>
        checkAgainstEndOfStream(stack)
          .map(Right(_))
          .getOrElse(Left(ParserException(expr.position, s"Unexpected ${expr.description}")))
      case expr1 :: _ :: _ =>
        checkAgainstEndOfStream(stack)
          .map(Right(_))
          .getOrElse(
            Left(
              failInfo
                .map(_.failure)
                .fold(ParserException(expr1.position, s"Unexpected ${expr1.description}"))(f =>
                  ParserException(f.failurePosition, f.formatError)
                )
            )
          )
    }
  }

  private def applyTokenToStack(token: Token, state: ParserState): ParserState = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, state.stack)
    val newState = state.copy(stack = newStack.toList)
    logger.debug(printState(newStack, newState.failInfo))
    val updatedStackOrErrors =
      firstValidOrAllInvalids(patterns)(_.instantiateOnStack(newStack)).leftMap(_.flatten)
    updatedStackOrErrors
      .fold(newState.tokenFailed, s => newState.tokenSucceed(foldStack(s)))
  }

  private def checkAgainstEndOfStream(stack: Stack): Option[Filter] = {
    import ExpressionPattern._
    firstValidOrAllInvalids(patterns)(_.instantiateOnStack(NonEmptyList(EndOfStream, stack)))
      .leftMap(_.flatten)
      .toOption
      .flatMap {
        case (filter: Filter) :: Nil => Option(filter)
        case _                       => None
      }
  }

  @tailrec
  private def foldStack(stack: Stack): Stack = {
    logger.debug(printStack(stack))
    import ExpressionPattern._
    val updatedStack = NonEmptyList.fromList(stack).flatMap { nonEmptyStack =>
      firstValidOrAllInvalids(patterns)(_.instantiateOnStack(nonEmptyStack)).toOption
    }
    updatedStack match {
      case None           => stack
      case Some(newStack) => foldStack(newStack)
    }
  }

  private def printState(stack: NonEmptyList[Expression], failureInfo: Option[ParserFailure]): String = {
    stack.toList.mkString("Stack: ", ",", "\n") ++ failureInfo.toString
  }
  private def printStack(stack: List[Expression]): String = stack.mkString("Folded stack: ", ",", "")
}
