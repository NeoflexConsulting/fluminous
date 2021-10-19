package org.fluminous.jq.filter.json.obj

import cats.syntax.traverse._
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

case class JsonObject(override val position: Int, values: Map[String, Filter]) extends Filter {
  override val isSingleValued: Boolean = true

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    values.toList.traverse { case (n, v) => v.transform(input).map(_.map(ev => (n, ev))) }.map(splitByObjects)
  }

  private def splitByObjects(lists: List[List[(String, Json)]]): List[Json] = {
    lists.foldLeft(List(io.circe.JsonObject.fromMap(Map.empty)))(addAttributeToObjects).map(Json.fromJsonObject)
  }

  private def addAttributeToObjects(
    objects: List[io.circe.JsonObject],
    attributeValues: List[(String, Json)]
  ): List[io.circe.JsonObject] = {
    objects.flatMap(obj => attributeValues.map(_ +: obj))
  }

  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
