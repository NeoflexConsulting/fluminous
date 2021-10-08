package org.fluminous.jq

import cats.data.NonEmptyList
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.ExpressionPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.{Token, Tokenizer}
import cats.syntax.foldable._
import cats.syntax.traverse._

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
        foldStack(updatedTokenizer, state.stack) match {
          case Left(ex) => Left(ex)
          case Right((tokenizerAfterFold, foldedStack)) =>
            getFilterFromStack(tokenizerAfterFold, foldedStack, state.failInfo)
        }

      case Right((updatedTokenizer, Some(token))) =>
        applyTokenToStack(token, updatedTokenizer, state) match {
          case Left(ex)                          => Left(ex)
          case Right((nextTokenizer, nextState)) => parse(nextTokenizer, nextState)
        }

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

  private def applyTokenToStack(
    token: Token,
    tokenizer: Tokenizer,
    state: ParserState
  ): Either[ParserException, (Tokenizer, ParserState)] = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, state.stack)
    val newState = state.copy(stack = newStack.toList)
    logger.debug(printState(newStack, newState.failInfo))
    for {
      (nextTokenizer, res) <- firstValidOrAllInvalidsWithEither(patterns, tokenizer)((a, d) =>
                               a.instantiateOnStack(d, newStack)
                             )
      flattenedRes = res.leftMap(_.flatten)
      e <- flattenedRes.fold(
            errors => Right((nextTokenizer, newState.tokenFailed(errors))),
            stack =>
              foldStack(nextTokenizer, stack).map {
                case (updatedTokenizer, updatedStack) => (updatedTokenizer, newState.tokenSucceed(updatedStack))
              }
          )
    } yield e
  }

  @tailrec
  private def foldStack(tokenizer: Tokenizer, stack: Stack): Either[ParserException, (Tokenizer, Stack)] = {
    logger.debug(printStack(stack))
    import ExpressionPattern._
    val updatedStack = NonEmptyList
      .fromList(stack)
      .map { nonEmptyStack =>
        firstValidOrAllInvalidsWithEither(patterns, tokenizer)((a, d) => a.instantiateOnStack(d, nonEmptyStack)).map {
          case (tokenizer, result) => result.toOption.map((tokenizer, _))
        }
      }
      .sequence
      .map(_.flatten)
    updatedStack match {
      case Left(ex)                               => Left(ex)
      case Right(None)                            => Right((tokenizer, stack))
      case Right(Some((nextTokenizer, newStack))) => foldStack(nextTokenizer, newStack)
    }
  }

  private def printState(stack: NonEmptyList[Expression], failureInfo: Option[ParserFailure]): String = {
    stack.toList.mkString("Stack: ", ",", "\n") ++ failureInfo.toString
  }
  private def printStack(stack: List[Expression]): String = stack.mkString("Folded stack: ", ",", "")
}
