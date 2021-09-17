package org.fluminous.jq.filter

import io.circe.Json
import org.fluminous.jq.Description

final case class Selector(override val position: Int, path: Seq[String]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    path.foldLeft(Option(input)) { (json, childName) =>
      json.flatMap(_.asObject) match {
        case None             => None
        case Some(jsonObject) => jsonObject(childName)
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
