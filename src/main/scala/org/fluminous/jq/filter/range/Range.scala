package org.fluminous.jq.filter.range

import io.circe.Json
import org.fluminous.jq.{ Description, Expression }

case class Range(override val position: Int, firstIndex: Option[Int], lastIndex: Option[Int]) extends Expression {

  override def toString: String = {
    s"${firstIndex.map(_.toString).getOrElse("")}:${lastIndex.map(_.toString).getOrElse("")}"
  }

  def start(arrayLength: Int): Int = {
    val f = firstIndex.getOrElse(0)
    if (f < 0) {
      arrayLength + f
    } else {
      f
    }
  }
  def end(arrayLength: Int): Int = {
    val l = lastIndex.getOrElse(arrayLength)
    if (l < 0) {
      arrayLength + l
    } else {
      l
    }
  }

  def rangeInterval: Int = {
    val f = firstIndex.getOrElse(0)
    val l = lastIndex.getOrElse(f + 1)
    l - f
  }
  override val description: String = Range.typeDescription.description
}

object Range {
  implicit def typeDescription: Description[Range] = new Description[Range] {
    override val description: String = "range"
  }
}
