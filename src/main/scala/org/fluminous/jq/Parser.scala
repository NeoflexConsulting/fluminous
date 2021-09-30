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
        val (nextTokenizer, nextState) = applyTokenToStack(token, updatedTokenizer, state)
        parse(nextTokenizer, nextState)
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
        Left(ParserException(expr.position, s"Unexpected ${expr.description}"))
      case expr1 :: (filter: Filter) :: _ =>
        Left(ParserException(expr1.position, s"Unexpected ${expr1.description}"))

      case expr1 :: _ :: _ =>
        Left(
          failInfo
            .map(_.failure)
            .fold(ParserException(expr1.position, s"Unexpected ${expr1.description}"))(f =>
              ParserException(f.failurePosition, f.formatError)
            )
        )
    }
  }

  private def applyTokenToStack(token: Token, tokenizer: Tokenizer, state: ParserState): (Tokenizer, ParserState) = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, state.stack)
    val newState = state.copy(stack = newStack.toList)
    logger.debug(printState(newStack, newState.failInfo))
    val (nextTokenizer, res) =
      firstValidOrAllInvalids(patterns, tokenizer)((a, d) => a.instantiateOnStack(d, newStack))
    val flattenedRes = res.leftMap(_.flatten)
    if (flattenedRes.isInvalid) {
      (nextTokenizer,newState.tokenFailed(flattenedRes.))
    }
      .fold((, s => newState.tokenSucceed(foldStack(nextTokenizer, s)))

  }

  @tailrec
  private def foldStack(tokenizer: Tokenizer, stack: Stack): (Tokenizer, Stack) = {
    logger.debug(printStack(stack))
    import ExpressionPattern._
    val updatedStack = NonEmptyList.fromList(stack).flatMap { nonEmptyStack =>
      val r = firstValidOrAllInvalids(patterns, tokenizer)((a, d) => a.instantiateOnStack(d, nonEmptyStack))
      r._2.toOption.map((r._1, _))
    }
    updatedStack match {
      case None                            => (tokenizer, stack)
      case Some((nextTokenizer, newStack)) => foldStack(nextTokenizer, newStack)
    }
  }

  private def printState(stack: NonEmptyList[Expression], failureInfo: Option[ParserFailure]): String = {
    stack.toList.mkString("Stack: ", ",", "\n") ++ failureInfo.toString
  }
  private def printStack(stack: List[Expression]): String = stack.mkString("Folded stack: ", ",", "")
}
