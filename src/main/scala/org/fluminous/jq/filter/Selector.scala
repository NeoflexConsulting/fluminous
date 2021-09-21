package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.{ Description, EvaluationException }

final case class Selector(override val position: Int, path: Seq[String]) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    val initial: Either[EvaluationException, Json] = Right(input)
    path.foldLeft(initial) { (json, childName) =>
      json.map(_.asObject) match {
        case Right(None) =>
          Left(EvaluationException(position, s"Field $childName is not json object"))
        case Right(Some(jsonObject)) =>
          jsonObject(childName).toRight(EvaluationException(position, s"Field $childName does not exist"))
      }
    }
  }
  override val description: String = s"selector for path: ${path.mkString("\\")}"
}

object Selector {
  implicit def typeDescription: Description[Selector] = new Description[Selector] {
    override val description: String = "field selector"
  }
}
