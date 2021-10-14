package org.fluminous.jq.filter.json.obj

import cats.syntax.traverse._
import io.circe.Json
import io.circe.Json.Null
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

case class JsonObject(override val position: Int, values: Map[String, Filter]) extends Filter {
  override val isSingleValued: Boolean = true

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    values.toList.traverse { case (n, v) => v.transform(input).map(j => flattenJson((n, j))) }.map(m =>
      List(Json.fromFields(m.filterNot(_._2.isNull)))
    )
  }

  private def flattenJson(pair: (String, List[Json])): (String, Json) = {
    (pair._1, pair._2 match {
      case h :: Nil => h
      case Nil      => Null
      case v @ _    => Json.fromValues(v)
    })
  }

  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
