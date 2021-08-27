package org.fluminous.jq

import scala.annotation.tailrec

case class Tokenizer(input: InputProvider) {

  def getNextToken(): Either[ParserException, (Tokenizer, Option[Token])] = {
    getNextToken(TokenizerState(input, None)).map(s => (Tokenizer(s._1.input), s._2))
  }

  @tailrec
  private def getNextToken(state: TokenizerState): Either[ParserException, (TokenizerState, Option[Token])] = {
    val nextSymbol = state.input.nextSymbol
    val result     = processSymbol(state, nextSymbol)
    (result, nextSymbol) match {
      case (ex @ Left(_), _)            => ex
      case (r @ Right(_), EOF)          => r
      case (r @ Right((_, Some(_))), _) => r
      case (Right((state, None)), _)    => getNextToken(state)
    }
  }

  private def processSymbol(
    state: TokenizerState,
    symbol: Symbol
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    state.currentToken.fold(setNewToken(state, symbol, None))(updateTokenOrCreateNew(state, symbol, _))
  }

  private def updateTokenOrCreateNew(
    state: TokenizerState,
    symbol: Symbol,
    token: Token
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    for {
      modifiedToken <- token.tryAppend(symbol, state.input.getPosition())
      newToken      <- modifiedToken.fold(setNewToken(state, symbol, Some(token)))(updateToken(state, _))
    } yield {
      newToken
    }
  }

  private def updateToken(
    state: TokenizerState,
    updatedToken: Token
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    Right((state.copy(currentToken = Some(updatedToken)), None))
  }

  private def setNewToken(
    state: TokenizerState,
    symbol: Symbol,
    previousToken: Option[Token]
  ): Either[ParserException, (TokenizerState, Option[Token])] = {
    Token.newToken(symbol, state.input.getPosition).map(t => (state.copy(currentToken = t), previousToken))
  }

  private case class TokenizerState(input: InputProvider, currentToken: Option[Token])
}
