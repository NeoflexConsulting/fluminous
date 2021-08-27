package org.fluminous.jq

sealed trait Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]]
}

object Token {
  def newToken(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) => tokenFromCharacter(c, position)
      case EOF          => Right(None)
    }
  }

  private def tokenFromCharacter(c: Char, position: Int): Either[ParserException, Option[Token]] = {
    if (Token.whitespaceSymbols.contains(c)) {
      Right(None)
    } else if (SpecialSymbol.symbols.contains(c)) Right(SpecialSymbol.symbols.get(c))
    else if (c == '"') Right(Some(RawString("")))
    else if (c.isDigit) Right(Some(IntegerNumber(c.toInt)))
    else if (c == Root.char) Right(Some(Root))
    else if (c.isLetter) Right(Some(Identifier(c.toString)))
    else Left(ParserException(position, s"Unsupported character $c"))
  }

  val whitespaceSymbols = Set(' ', '\n', '\r', '\t')
}

sealed trait SpecialSymbol extends Token {
  val char: Char
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = Right(None)
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
    OptionalSign
  ).map(s => (s.char, s)).toMap
}

case object Root extends Token {
  val char = '.'
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF            => Right(None)
      case Character('.') => Right(Some(RecursiveDescent))
      case _              => Right(None)
    }
  }
}
case class Identifier(value: String) extends Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case EOF =>
        Right(None)
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case Character(c) =>
        Right(Some(Identifier(value :+ c)))
    }
  }
}

case class RawString(value: String) extends Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character('"') => Right(None)
      case Character(c)   => Right(Some(RawString(value :+ c)))
      case EOF            => Left(ParserException(position, s"String $value doesn't end with quote"))
    }
  }
}

case class IntegerNumber(value: Int) extends Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case Character(c) if c.isDigit =>
        Right(Some(IntegerNumber(value * 10 + c.toInt)))
      case Character('.') =>
        Right(Some(FloatNumber(value)))
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(position, "Identifier could not start with number. Try to surround it by quotes"))

    }
  }
}
case class FloatNumber(value: BigDecimal, fractionalNumber: Int = 0) extends Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || SpecialSymbol.symbols.contains(c) =>
        Right(None)
      case Character(c) if c.isDigit =>
        Right(
          Some(FloatNumber(value + c.toInt * value * BigDecimal(0.1).pow(fractionalNumber + 1), fractionalNumber + 1))
        )
      case EOF =>
        Right(None)
      case _ =>
        Left(ParserException(position, "Identifier could not start with number. Try to surround it by quotes"))

    }
  }
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
case object RecursiveDescent extends Token {
  def tryAppend(symbol: Symbol, position: Int): Either[ParserException, Option[Token]] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || c == Pipe.char =>
        Right(None)
      case EOF =>
        Right(None)
      case Character(c) =>
        Left(ParserException(position, s"""Invalid sequence "..$c""""))
    }
  }
}
