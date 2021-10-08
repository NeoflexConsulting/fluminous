package org.fluminous.jq.tokens

import org.fluminous.jq
import org.fluminous.jq.ParserException
import org.fluminous.jq.input.{ EOF, InputProvider }

import scala.annotation.tailrec

case class Tokenizer(input: InputProvider, buffer: Vector[Token] = Vector.empty) {
  def nextToken: Either[ParserException, (Tokenizer, Option[Token])] = {
    takeFromBuffer().getOrElse(
      getNextToken(TokenizerState(input, None)).map { case (state, token) => (Tokenizer(state.input, buffer), token) }
    )
  }

  def lookupToken(index: Int): Either[ParserException, (Tokenizer, Option[Token])] = {
    buffer
      .lift(index)
      .fold {
        for {
          (tokenizer, tokens) <- getTokens(buffer.length - index + 1)
          updatedBuffer       = buffer ++ tokens
        } yield (Tokenizer(tokenizer.input, updatedBuffer), updatedBuffer.lift(index))
      } { token =>
        Right((this, Some(token)))
      }
  }

  def allTokens: Either[ParserException, Seq[Token]] = {
    collectTokens(this, Seq.empty).map(_.reverse)
  }

  private def getTokens(quantity: Int): Either[ParserException, (Tokenizer, Vector[Token])] = {
    import cats.syntax.foldable._
    List.range(0, quantity).foldLeftM((this, Vector.empty[Token])) {
      case (state, _) =>
        val (tokenizer, tokens) = state
        tokenizer.nextToken.map {
          case (nextTokenizer, token) =>
            (nextTokenizer, tokens ++ token.toSeq)
        }
    }
  }

  private def takeFromBuffer(): Option[Either[ParserException, (Tokenizer, Option[Token])]] = {
    buffer.headOption.map(t => Right((Tokenizer(this.input, buffer.tail), Some(t))))
  }

  @tailrec
  private def collectTokens(tokenizer: Tokenizer, tokens: Seq[Token]): Either[ParserException, Seq[Token]] = {
    tokenizer.nextToken match {
      case Left(ex)                    => Left(ex)
      case Right((_, None))            => Right(tokens)
      case Right((tokenizer, Some(t))) => collectTokens(tokenizer, t +: tokens)
    }
  }

  @tailrec
  private def getNextToken(state: TokenizerState): Either[ParserException, (TokenizerState, Option[Token])] = {
    val nextSymbol = state.input.nextSymbol
    val result     = processSymbol(state, nextSymbol)
    (result, nextSymbol) match {
      case (ex @ Left(_), _)            => ex
      case (r @ Right(_), EOF)          => r
      case (r @ Right((_, Some(_))), _) => r
      case (Right((state, None)), _)    => getNextToken(state.moveToNextSymbol)
    }
  }

  private def processSymbol(
    state: TokenizerState,
    symbol: jq.input.Symbol
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    state.currentToken.fold(setNewToken(state, symbol, None))(updateTokenOrCreateNew(state, symbol, _))
  }

  private def updateTokenOrCreateNew(
    state: TokenizerState,
    symbol: jq.input.Symbol,
    token: Token
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    for {
      modifiedToken <- token.tryAppend(symbol, state.input.position)
      newToken      <- modifiedToken.fold(setNewToken(state, symbol, Some(token)))(updateToken(state, _))
    } yield {
      newToken
    }
  }

  private def updateToken(
    state: TokenizerState,
    updatedToken: Token
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    Right((state.setCurrentToken(updatedToken), None))
  }

  private def setNewToken(
    state: TokenizerState,
    symbol: jq.input.Symbol,
    previousToken: Option[Token]
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    Token.newToken(symbol, state.input.position).map(t => (state.setCurrentToken(t), previousToken))
  }

  private case class TokenizerState(input: InputProvider, currentToken: Option[Token]) {
    def setCurrentToken(t: Option[Token]): TokenizerState = {
      this.copy(currentToken = t)
    }
    def setCurrentToken(t: Token): TokenizerState = {
      this.copy(currentToken = Option(t))
    }
    def moveToNextSymbol: TokenizerState = {
      this.copy(input = input.moveToNext())
    }
  }
}
