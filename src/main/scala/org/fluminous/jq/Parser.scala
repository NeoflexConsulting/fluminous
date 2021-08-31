package org.fluminous.jq

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.FilterPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.Token
import scala.annotation.tailrec
import cats.syntax.foldable._
import cats.instances.list._

trait Parser {
  type Stack = List[Expression]
  def parse(input: InputProvider): Either[ParserException, Filter] = {
    parse(Tokenizer(input))
  }
  @tailrec
  private def parse(
    tokenizer: Tokenizer,
    stack: Stack = List.empty
  ): Either[ParserException, Filter] = {
    tokenizer.nextToken match {
      case Left(ex) =>
        Left(ex)
      case Right((updatedTokenizer, None)) =>
        getFilterFromStack(updatedTokenizer, foldStack(stack))
      case Right((updatedTokenizer, Some(token))) =>
        parse(updatedTokenizer, applyTokenToStack(token, tokenizer, stack))
    }
  }

  private def getFilterFromStack(tokenizer: Tokenizer, stack: Stack): Either[ParserException, Filter] = {
    stack match {
      case Nil =>
        Left(ParserException(tokenizer.input.position, "Invalid input: empty"))
      case (filter: Filter) :: Nil =>
        Right(filter)
      case expr1 :: _ =>
        Left(ParserException(tokenizer.input.position, s"Invalid input: $expr1"))
    }
  }

  private def applyTokenToStack(token: Token, tokenizer: Tokenizer, stack: Stack): Stack = {
    import FilterPattern._
    val newStack = token +: stack
    patterns.foldMapK(_.instantiateOnStack(newStack).map(foldStack).getOrElse(newStack))
  }

  @tailrec
  private def foldStack(stack: Stack): Stack = {
    import FilterPattern._
    patterns.foldMapK(_.instantiateOnStack(stack)) match {
      case None           => stack
      case Some(newStack) => foldStack(newStack)
    }
  }
}
