package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

final case class Selector(override val position: Int, field: String) extends Filter {
  override def transform(input: Json): Either[EvaluationException, Json] = {
    for {
      jsonObject <- input.asObject.toRight(EvaluationException(position, s"Field $field is not json object"))
      result     <- jsonObject(field).toRight(EvaluationException(position, s"Field $field does not exist"))
    } yield result
  }
  override val description: String = s"selector for field: ${field.mkString("\\")}"
}

object Selector {
  implicit def typeDescription: Description[Selector] = new Description[Selector] {
    override val description: String = "field selector"
  }
  implicit def identityDescription: Description[IdentitySelector] = new Description[IdentitySelector] {
    override val description: String = "field selector"
  }
}
