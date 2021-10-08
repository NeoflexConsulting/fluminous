package org.fluminous.jq.tokens

import org.fluminous.jq.{ input, Description, ParserException }

sealed trait SpecialSymbol extends BasicToken {
  override val char: Char
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = Right(TokenConstructed)
}

object SpecialSymbol {
  val symbols: Map[Char, Int => SpecialSymbol] = Map[Char, Int => SpecialSymbol](
    VerticalSlash.char      -> (VerticalSlash(_)),
    LeftBracket.char        -> (LeftBracket(_)),
    RightBracket.char       -> (RightBracket(_)),
    LeftSquareBracket.char  -> (LeftSquareBracket(_)),
    RightSquareBracket.char -> (RightSquareBracket(_)),
    LeftFigureBracket.char  -> (LeftFigureBracket(_)),
    RightFigureBracket.char -> (RightFigureBracket(_)),
    Colon.char              -> (Colon(_)),
    OptionalSign.char       -> (OptionalSign(_)),
    Comma.char              -> (Comma(_))
  )
}

case class VerticalSlash(override val position: Int) extends SpecialSymbol {
  val char                         = VerticalSlash.char
  override val description: String = VerticalSlash.typeDescription.description
}
object VerticalSlash {
  val char = '|'
  implicit def typeDescription: Description[VerticalSlash] = new Description[VerticalSlash] {
    override val description: String = char.toString
  }
}

case class LeftBracket(override val position: Int) extends SpecialSymbol {
  val char                         = LeftBracket.char
  override val description: String = LeftBracket.typeDescription.description

}
object LeftBracket {
  val char = '('
  implicit def typeDescription: Description[LeftBracket] = new Description[LeftBracket] {
    override val description: String = char.toString
  }
}

case class RightBracket(override val position: Int) extends SpecialSymbol {
  val char                         = RightBracket.char
  override val description: String = RightBracket.typeDescription.description

}
object RightBracket {
  val char = ')'
  implicit def typeDescription: Description[RightBracket] = new Description[RightBracket] {
    override val description: String = char.toString
  }
}

case class LeftSquareBracket(override val position: Int) extends SpecialSymbol {
  val char                         = LeftSquareBracket.char
  override val description: String = LeftSquareBracket.typeDescription.description

}
object LeftSquareBracket {
  val char = '['
  implicit def typeDescription: Description[LeftSquareBracket] = new Description[LeftSquareBracket] {
    override val description: String = char.toString
  }
}

case class RightSquareBracket(override val position: Int) extends SpecialSymbol {
  val char                         = RightSquareBracket.char
  override val description: String = RightSquareBracket.typeDescription.description

}
object RightSquareBracket {
  val char = ']'
  implicit def typeDescription: Description[RightSquareBracket] = new Description[RightSquareBracket] {
    override val description: String = char.toString
  }
}

case class LeftFigureBracket(override val position: Int) extends SpecialSymbol {
  val char                         = LeftFigureBracket.char
  override val description: String = LeftFigureBracket.typeDescription.description

}
object LeftFigureBracket {
  val char = '{'
  implicit def typeDescription: Description[LeftFigureBracket] = new Description[LeftFigureBracket] {
    override val description: String = char.toString
  }
}

case class RightFigureBracket(override val position: Int) extends SpecialSymbol {
  val char                         = RightFigureBracket.char
  override val description: String = RightFigureBracket.typeDescription.description

}
object RightFigureBracket {
  val char = '}'
  implicit def typeDescription: Description[RightFigureBracket] = new Description[RightFigureBracket] {
    override val description: String = char.toString
  }
}

case class Colon(override val position: Int) extends SpecialSymbol {
  val char                         = Colon.char
  override val description: String = Colon.typeDescription.description

}
object Colon {
  val char = ':'
  implicit def typeDescription: Description[Colon] = new Description[Colon] {
    override val description: String = char.toString
  }
}

case class OptionalSign(override val position: Int) extends SpecialSymbol {
  val char                         = OptionalSign.char
  override val description: String = OptionalSign.typeDescription.description

}
object OptionalSign {
  val char = '?'
  implicit def typeDescription: Description[OptionalSign] = new Description[OptionalSign] {
    override val description: String = char.toString
  }
}

case class Comma(override val position: Int) extends SpecialSymbol {
  val char                         = Comma.char
  override val description: String = Comma.typeDescription.description

}
object Comma {
  val char = ','
  implicit def typeDescription: Description[Comma] = new Description[Comma] {
    override val description: String = char.toString
  }
}
