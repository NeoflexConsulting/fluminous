package org.fluminous.jq

trait Expression {
  val position: Int
  val description: String
}

object EndOfStream extends Expression {
  val position: Int       = 0
  val description: String = "EndOfStream"
}
