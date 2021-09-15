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
  private def parse(tokenizer: Tokenizer, stack: Stack = List.empty): Either[ParserException, Filter] = {
    tokenizer.nextToken match {
      case Left(ex) =>
        Left(ex)
      case Right((updatedTokenizer, None)) =>
        getFilterFromStack(updatedTokenizer, foldStack(stack))
      case Right((updatedTokenizer, Some(token))) =>
        parse(updatedTokenizer, applyTokenToStack(token, stack))
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

  private def applyTokenToStack(token: Token, stack: Stack): Stack = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, stack)
    logger.debug(printStack(newStack))
    patterns.foldMapK(_.instantiateOnStack(newStack)).map(foldStack).getOrElse(newStack.toList)
  }

  @tailrec
  private def foldStack(stack: Stack): Stack = {
    logger.debug(printStack(stack))
    import ExpressionPattern._
    val updatedStack = NonEmptyList.fromList(stack).flatMap { nonEmptyStack =>
      patterns.foldMapK(_.instantiateOnStack(nonEmptyStack))
    }
    updatedStack match {
      case None           => stack
      case Some(newStack) => foldStack(newStack)
    }
  }

  private def printStack(stack: NonEmptyList[Expression]): String = stack.toList.mkString("Stack: ", ",", "")
  private def printStack(stack: List[Expression]): String         = stack.mkString("Stack: ", ",", "")
}
