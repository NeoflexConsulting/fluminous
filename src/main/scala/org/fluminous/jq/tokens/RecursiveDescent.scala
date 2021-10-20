package org.fluminous.jq.tokens

import cats.data.Chain
import io.circe.Json.Folder
import io.circe.{ Json, JsonNumber, JsonObject }
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ input, Description, EvaluationException, ParserException }
import org.fluminous.jq.input.{ Character, EOF }
import org.fluminous.jq.tokens.symbolic.VerticalSlash
import io.circe.syntax._
import cats.syntax.foldable._

case class RecursiveDescent(override val position: Int) extends Token with Filter {
  override val isSingleValued: Boolean = false
  def tryAppend(symbol: input.Symbol, position: Int): Either[ParserException, AppendResult] = {
    symbol match {
      case Character(c) if Token.whitespaceSymbols.contains(c) || c == VerticalSlash.char =>
        Right(TokenConstructed)
      case EOF =>
        Right(TokenConstructed)
      case Character(c) =>
        Left(ParserException(position, s"""Invalid sequence "..$c""""))
    }
  }
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    Right(input.foldWith(jsonFolder).toList)
  }

  private val jsonFolder: Folder[Chain[Json]] = new Folder[Chain[Json]] {
    override def onNull: Chain[Json] = Chain.empty

    override def onBoolean(value: Boolean): Chain[Json] = Chain(value.asJson)

    override def onNumber(value: JsonNumber): Chain[Json] = Chain(Json.fromJsonNumber(value))

    override def onString(value: String): Chain[Json] = Chain(Json.fromString(value))

    override def onArray(value: Vector[Json]): Chain[Json] = {
      Json.fromValues(value) +: value.foldMap(_.foldWith(jsonFolder))
    }

    override def onObject(value: JsonObject): Chain[Json] = {
      Json.fromJsonObject(value) +: value.values.map(_.foldWith(jsonFolder)).fold(Chain.empty)(_ ++ _)
    }
  }

  override val description: String = """.."""
  implicit def typeDescription: Description[RecursiveDescent] = new Description[RecursiveDescent] {
    override val description: String = toString
  }
}
