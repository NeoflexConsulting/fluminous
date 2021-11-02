package org.fluminous.jq.filter.json.obj

import cats.syntax.traverse._
import cats.syntax.apply._
import io.circe.Json
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, EvaluationException }

import scala.collection.immutable.ListMap

case class JsonObject(override val position: Int, values: ListMap[Filter, Filter]) extends Filter {
  override val isSingleValued: Boolean = true

  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    values.toList.traverse { case (n, v) => (n.transform(input).flatMap(castAsString), v.transform(input)).tupled }
      .map(splitByObjects)
  }

  private def castAsString(jsons: List[Json]): Either[EvaluationException, List[String]] = {
    jsons
      .map(v =>
        v.asString.toRight(EvaluationException(position, s"attribute name must be string, but evaluated to ${v.name}"))
      )
      .sequence
  }

  private def splitByObjects(lists: List[(List[String], List[Json])]): List[Json] = {
    lists.foldLeft(List(io.circe.JsonObject.fromMap(Map.empty)))(addAttributeToObjects).map(Json.fromJsonObject)
  }

  private def addAttributeToObjects(
    objects: List[io.circe.JsonObject],
    attributeValues: (List[String], List[Json])
  ): List[io.circe.JsonObject] = {
    objects.flatMap(obj => attributeValues.tupled.map(v => obj.add(v._1, v._2)))
  }

  override val description: String = JsonObject.typeDescription.description
}

object JsonObject {
  implicit def typeDescription: Description[JsonObject] = new Description[JsonObject] {
    override val description: String = "end of json object"
  }
}
