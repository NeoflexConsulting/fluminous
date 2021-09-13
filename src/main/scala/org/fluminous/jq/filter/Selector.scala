package org.fluminous.jq.filter

import io.circe.Json

final case class Selector(path: Seq[String]) extends Filter {
  override def transform(input: Json): Option[Json] = {
    path.foldLeft(Option(input)) { (json, childName) =>
      json.flatMap(_.asObject) match {
        case None             => None
        case Some(jsonObject) => jsonObject(childName)
      }
    }
  }

  override def toString: String = path.mkString(".", "|", "")
}
