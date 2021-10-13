package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }
import org.fluminous.jq.filter.SequenceFilter
import cats.syntax.traverse._

case class Splitter(override val position: Int) extends SequenceFilter {

  override def transform(input: List[Json]): Either[EvaluationException, List[Json]] = {
    input.map(splitJson).flatSequence
  }

  private def splitJson(json: Json): Either[EvaluationException, List[Json]] = {
    json.asArray
      .map(_.toList)
      .orElse(json.asObject.map(_.toList.map(_._2)))
      .toRight(
        EvaluationException(
          position,
          s"Trying to split by elements json of type ${json.name}"
        )
      )
  }
  override val description: String = s"splitter"
}

object Splitter {
  implicit def typeDescription: Description[Splitter] = new Description[Splitter] {
    override val description: String = "splitter"
  }
}
