package org.fluminous.jq.tokens

import org.fluminous.jq.Expression
import org.fluminous.jq.input.{ Character, EOF }
import org.fluminous.jq.{ input, ParserException }

trait Token extends Expression {
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]]
}

object Token {
  def newToken(symbol: input.Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) => tokenFromCharacter(c, position)
      case EOF          => Right(None)
    }
  }

  private def tokenFromCharacter(c: Char, position: Int): Either[ParserException, Option[Token]] = {
    if (Token.whitespaceSymbols.contains(c)) {
      Right(None)
    } else if (SpecialSymbol.symbols.contains(c)) Right(SpecialSymbol.symbols.get(c).map(_(position)))
    else if (c == '"') Right(Some(RawString(position, "", false)))
    else if (c.isDigit) Right(Some(IntegerNumber(position, c.toString)))
    else if (c == Root.char) Right(Some(Root(position)))
    else if (c.isLetter) Right(Some(Identifier(position, c.toString)))
    else if (c == '-') Right(Some(IntegerNumber(position, "-")))
    else Left(ParserException(position, s"Unsupported character $c"))
  }

  val whitespaceSymbols = Set(' ', '\n', '\r', '\t')
}
