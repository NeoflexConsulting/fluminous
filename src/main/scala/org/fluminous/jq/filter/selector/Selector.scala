package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, EvaluationException}

final case class Selector(override val position: Int, path: String) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    input.asObject match {
      case None =>
        Left(EvaluationException(position, s"Field $path is not json object"))
      case Some(jsonObject) =>
        jsonObject(path).toRight(EvaluationException(position, s"Field $path does not exist"))
    }
  }
  override val description: String = s"selector for path: ${path.mkString("\\")}"
}

object Selector {
  implicit def typeDescription: Description[Selector] = new Description[Selector] {
    override val description: String = "field selector"
  }
  implicit def identityDescription: Description[IdentitySelector] = new Description[IdentitySelector] {
    override val description: String = "field selector"
  }
}
