package org.fluminous.jq

import cats.data.NonEmptyList
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.ExpressionPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.Token

import scala.annotation.tailrec
import cats.syntax.foldable._
import cats.instances.list._
import org.slf4j.LoggerFactory

trait Parser {
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
        getFilterFromStack(updatedTokenizer, foldStack(state.stack))
      case Right((updatedTokenizer, Some(token))) =>
        parse(updatedTokenizer, applyTokenToStack(token, state))
    }
  }

  private def getFilterFromStack(tokenizer: Tokenizer, stack: Stack): Either[ParserException, Filter] = {
    stack match {
      case Nil =>
        Left(ParserException(tokenizer.input.position, "Invalid input: empty"))
      case (filter: Filter) :: Nil =>
        Right(filter)
      case expr1 :: _ =>
        Left(ParserException(tokenizer.input.position, s"Invalid input: '$expr1'"))
    }
  }

  private def applyTokenToStack(token: Token, state: ParserState): ParserState = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, state.stack)
    logger.debug(printStack(newStack))
    val updatedStackOrErrors =
      patterns.foldMapA(_.instantiateOnStack(newStack).leftMap(List(_)))
    updatedStackOrErrors
      .leftMap(ParserFailure(_))
      .fold(state.tokenFailed, s => state.tokenSucceed(foldStack(s)))
  }

  @tailrec
  private def foldStack(stack: Stack): Stack = {
    logger.debug(printStack(stack))
    import ExpressionPattern._
    val updatedStack = NonEmptyList.fromList(stack).flatMap { nonEmptyStack =>
      patterns.foldMapA(_.instantiateOnStack(nonEmptyStack).leftMap(List(_))).toOption
    }
    updatedStack match {
      case None           => stack
      case Some(newStack) => foldStack(newStack)
    }
  }

  private def printStack(stack: NonEmptyList[Expression]): String = stack.toList.mkString("Stack: ", ",", "")
  private def printStack(stack: List[Expression]): String         = stack.mkString("Stack: ", ",", "")
}
