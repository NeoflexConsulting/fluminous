package org.fluminous.jq.tokens.symbolic

import io.circe.{Json, JsonObject}
import org.fluminous.jq.filter.algebra.AlgebraOperation
import org.fluminous.jq.tokens.{AppendResult, TokenConstructed}
import org.fluminous.jq.{Description, EvaluationException, ParserException, input}

trait AtomicToken extends SymbolToken {
  override val char: Char
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = Right(TokenConstructed)
}

object AtomicToken {
  val symbols: Map[Char, Int => AtomicToken] = Map[Char, Int => AtomicToken](
    VerticalSlash.char      -> (VerticalSlash(_)),
    LeftBracket.char        -> (LeftBracket(_)),
    RightBracket.char       -> (RightBracket(_)),
    LeftSquareBracket.char  -> (LeftSquareBracket(_)),
    RightSquareBracket.char -> (RightSquareBracket(_)),
    LeftFigureBracket.char  -> (LeftFigureBracket(_)),
    RightFigureBracket.char -> (RightFigureBracket(_)),
    Colon.char              -> (Colon(_)),
    OptionalSign.char       -> (OptionalSign(_)),
    Comma.char              -> (Comma(_)),
    Plus.char               -> (Plus(_)),
    Minus.char              -> (Minus(_)),
    Divide.char             -> (Divide(_)),
    Multiply.char           -> (Multiply(_)),
    Modulo.char             -> (Modulo(_))
  )
}
