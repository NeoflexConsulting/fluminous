package org.fluminous.jq

import cats.data.NonEmptyList
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.pattern.ExpressionPattern
import org.fluminous.jq.input.InputProvider
import org.fluminous.jq.tokens.Token

import scala.annotation.tailrec
import cats.syntax.foldable._
import cats.instances.list._
import io.circe.Decoder.state
import org.slf4j.LoggerFactory
import ru.CryptoPro.JCP.ASN.PKIXCMP.PKIFailureInfo

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
        Left(ParserException(tokenizer.input.position, "Invalid input: empty"))
      case (filter: Filter) :: Nil =>
        Right(filter)
      case _ :: _ =>
        Left(
          failInfo.fold(ParserException(tokenizer.input.position, "Unknown parsing error"))(f =>
            ParserException(f.position, f.formatError)
          )
        )
    }
  }

  private def applyTokenToStack(token: Token, state: ParserState): ParserState = {
    import ExpressionPattern._
    val newStack = NonEmptyList(token, state.stack)
    logger.debug(printState(newStack, state.failInfo))
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

  private def printState(stack: NonEmptyList[Expression], failureInfo: Option[ParserFailure]): String = {
    stack.toList.mkString("Stack: ", ",", "\n") ++ failureInfo.toString
  }
  private def printStack(stack: List[Expression]): String = stack.mkString("Folded stack: ", ",", "")
}
