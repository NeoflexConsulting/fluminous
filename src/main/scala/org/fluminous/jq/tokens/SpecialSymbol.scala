package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, ParserException }

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
trait Pipe                     extends SpecialSymbol { val char = '|' }
case object Pipe               extends Pipe
trait LeftBracket              extends SpecialSymbol { val char = '(' }
case object LeftBracket        extends LeftBracket
trait RightBracket             extends SpecialSymbol { val char = ')' }
case object RightBracket       extends RightBracket
trait LeftSquareBracket        extends SpecialSymbol { val char = '[' }
case object LeftSquareBracket  extends LeftSquareBracket
trait RightSquareBracket       extends SpecialSymbol { val char = ']' }
case object RightSquareBracket extends RightSquareBracket
trait LeftFigureBracket        extends SpecialSymbol { val char = '{' }
case object LeftFigureBracket  extends LeftFigureBracket
trait RightFigureBracket       extends SpecialSymbol { val char = '}' }
case object RightFigureBracket extends RightFigureBracket
trait Colon                    extends SpecialSymbol { val char = ':' }
case object Colon              extends Colon
trait OptionalSign             extends SpecialSymbol { val char = '?' }
case object OptionalSign       extends OptionalSign
trait Comma                    extends SpecialSymbol { val char = ',' }
case object Comma              extends Comma
