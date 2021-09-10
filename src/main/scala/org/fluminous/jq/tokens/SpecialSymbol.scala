package org.fluminous.jq.tokens

import org.fluminous.jq.{ParserException, input}

sealed trait SpecialSymbol extends BasicToken {
  override val char: Char
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = Right(None)
}

object SpecialSymbol {
  val symbols: Map[Char, SpecialSymbol] = Seq[SpecialSymbol](
    Pipe,
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    LeftFigureBracket,
    RightFigureBracket,
    Colon,
    OptionalSign,
    Comma
  ).map(s => (s.char, s)).toMap
}

case object Pipe               extends SpecialSymbol { val char = '|' }
case object LeftBracket        extends SpecialSymbol { val char = '(' }
case object RightBracket       extends SpecialSymbol { val char = ')' }
case object LeftSquareBracket  extends SpecialSymbol { val char = '[' }
case object RightSquareBracket extends SpecialSymbol { val char = ']' }
case object LeftFigureBracket  extends SpecialSymbol { val char = '{' }
case object RightFigureBracket extends SpecialSymbol { val char = '}' }
case object Colon              extends SpecialSymbol { val char = ':' }
case object OptionalSign       extends SpecialSymbol { val char = '?' }
case object Comma              extends SpecialSymbol { val char = ',' }
