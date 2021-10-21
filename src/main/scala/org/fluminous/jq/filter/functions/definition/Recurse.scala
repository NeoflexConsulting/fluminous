package org.fluminous.jq.filter.functions.definition

import io.circe.Json
import org.fluminous.jq.{ EvaluationException, Parser }
import org.fluminous.jq.filter.Filter

import scala.annotation.tailrec

object Recurse extends JqFunction with Parser {
  override val name: String = "recurse"
  val minParameters: Int    = 0
  val maxParameters: Int    = 2
  val defaultParameters: Map[Int, Filter] = Map(
    0 -> parse(".[]?").right.get,
    1 -> parse(". != null").right.get
  )
  override val isSingledValue: Boolean = false
  override def invoke(input: Json, parameters: List[Filter], position: Int): Either[EvaluationException, List[Json]] = {
    val filter    = parameters.lift(0).getOrElse(defaultParameters(0))
    val condition = parameters.lift(1).getOrElse(defaultParameters(1))
    recurse(List(input), filter, condition, position, List.empty).map(_.reverse)
  }

  @tailrec
  private def recurse(
    stack: List[Json],
    filter: Filter,
    condition: Filter,
    position: Int,
    output: List[Json]
  ): Either[EvaluationException, List[Json]] = {
    stack match {
      case Nil => Right(output)
      case head :: rest =>
        evalNextInputs(head, filter, condition, position) match {
          case Left(ex)             => Left(ex)
          case Right((false, list)) => recurse(list ++ rest, filter, condition, position, output)
          case Right((true, list))  => recurse(list ++ rest, filter, condition, position, head +: output)
        }
    }
  }

  private def evalNextInputs(
    input: Json,
    filter: Filter,
    condition: Filter,
    position: Int
  ): Either[EvaluationException, (Boolean, List[Json])] = {
    for {
      evCondition <- condition.transformToSingleJson(input)
      bValue <- evCondition.asBoolean.toRight(
                 EvaluationException(
                   position,
                   s"Condition in recurse should be boolean, but received ${evCondition.name}"
                 )
               )
      evResult <- if (bValue) filter.transform(input) else Right(List.empty)
    } yield (bValue, evResult.filterNot(_.isNull))
  }
}
