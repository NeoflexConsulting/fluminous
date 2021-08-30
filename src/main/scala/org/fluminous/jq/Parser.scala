package org.fluminous.jq

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.FilterPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.Token
import scala.annotation.tailrec

trait Parser {
  def parse(input: InputProvider): Either[ParserException, Filter] = {
    parse(Tokenizer(input))
  }

  @tailrec
  private def parse(
    tokenizer: Tokenizer,
    parserState: Either[ParserException, ParserState] = Right(ParserState())
  ): Either[ParserException, Filter] = {
    (tokenizer.nextToken, parserState) match {
      case (Left(ex), _) =>
        Left(ex)
      case (_, Left(ex)) =>
        Left(ex)
      case (Right((updatedTokenizer, None)), Right(state)) =>
        getFilterFromStack(updatedTokenizer, state)
      case (Right((updatedTokenizer, Some(token))), Right(state)) =>
        parse(updatedTokenizer, applyTokenToStack(token, tokenizer, state))

    }
  }
  private def getFilterFromStack(tokenizer: Tokenizer, state: ParserState): Either[ParserException, Filter] = {
    state.stack match {
      case Nil =>
        Left(ParserException(tokenizer.input.position, "Invalid input: empty"))
      case (filter: Filter) :: Nil =>
        Right(filter)
      case expr1 :: _ =>
        Left(ParserException(tokenizer.input.position, s"Invalid input: s$expr1"))
    }
  }

  private def applyTokenToStack(
    token: Token,
    tokenizer: Tokenizer,
    state: ParserState
  ): Either[ParserException, ParserState] = {
    val newStack          = token +: state.stack
    val newFilterPatterns = state.filterPatterns.filter(_.isSuitableForStack(newStack))
    newFilterPatterns match {
      case Nil =>
        Left(ParserException(tokenizer.input.position, s"Invalid input: ${state.stack.mkString}"))
      case pattern :: Nil =>
        Right(ParserState(FilterPattern.patterns, pattern.instantiateOnStack(newStack)))
      case _ :: _ =>
        Right(ParserState(newFilterPatterns, newStack))
    }
  }

  private case class ParserState(
    filterPatterns: Seq[FilterPattern] = FilterPattern.patterns,
    stack: List[Expression] = List.empty)

}
