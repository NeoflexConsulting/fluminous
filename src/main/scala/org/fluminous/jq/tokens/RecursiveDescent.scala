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

import scala.annotation.tailrec

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
    recurse(List(input), List(input)).map(_.reverse)
  }

  @tailrec
  private def recurse(stack: List[Json], output: List[Json]): Either[EvaluationException, List[Json]] = {
    println("########## " + stack + "  : " + output)
    stack match {
      case Nil => Right(output)
      case head :: rest =>
        val nextJsons = evalNextInputs(head)
        recurse(nextJsons ++ rest, nextJsons ++ output)
    }
  }

  private def evalNextInputs(input: Json): List[Json] = {
    input.fold(
      List.empty[Json],
      _ => List.empty[Json],
      _ => List.empty[Json],
      _ => List.empty[Json],
      array => array.toList,
      obj => obj.values.toList
    )
  }

  override val description: String = """.."""
  implicit def typeDescription: Description[RecursiveDescent] = new Description[RecursiveDescent] {
    override val description: String = toString
  }
}
