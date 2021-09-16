package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }

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


trait Pipe extends SpecialSymbol {
  val char                         = '|'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[Pipe] = new Description[Pipe] {
    override val description: String = char.toString
  }
}
case object Pipe extends Pipe


trait LeftBracket extends SpecialSymbol {
  val char                         = '('
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[LeftBracket] = new Description[LeftBracket] {
    override val description: String = char.toString
  }
}
case object LeftBracket extends LeftBracket


trait RightBracket extends SpecialSymbol {
  val char                         = ')'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[RightBracket] = new Description[RightBracket] {
    override val description: String = char.toString
  }
}
case object RightBracket extends RightBracket


trait LeftSquareBracket extends SpecialSymbol {
  val char                         = '['
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[LeftSquareBracket] = new Description[LeftSquareBracket] {
    override val description: String = char.toString
  }
}
case object LeftSquareBracket extends LeftSquareBracket


trait RightSquareBracket extends SpecialSymbol {
  val char                         = ']'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[RightSquareBracket] = new Description[RightSquareBracket] {
    override val description: String = char.toString
  }
}
case object RightSquareBracket extends RightSquareBracket


trait LeftFigureBracket extends SpecialSymbol {
  val char                         = '{'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[LeftFigureBracket] = new Description[LeftFigureBracket] {
    override val description: String = char.toString
  }
}
case object LeftFigureBracket extends LeftFigureBracket


trait RightFigureBracket extends SpecialSymbol {
  val char                         = '}'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[RightFigureBracket] = new Description[RightFigureBracket] {
    override val description: String = char.toString
  }
}
case object RightFigureBracket extends RightFigureBracket


trait Colon extends SpecialSymbol {
  val char                         = ':'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[Colon] = new Description[Colon] {
    override val description: String = char.toString
  }
}
case object Colon extends Colon

trait OptionalSign extends SpecialSymbol {
  val char                         = '?'
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[OptionalSign] = new Description[OptionalSign] {
    override val description: String = char.toString
  }
}
case object OptionalSign extends OptionalSign


trait Comma extends SpecialSymbol {
  val char                         = ','
  override val description: String = typeDescription.description
  implicit def typeDescription: Description[Comma] = new Description[Comma] {
    override val description: String = char.toString
  }
}
case object Comma extends Comma
