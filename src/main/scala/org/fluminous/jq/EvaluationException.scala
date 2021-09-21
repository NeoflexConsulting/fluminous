package org.fluminous.jq

case class EvaluationException(position: Int, message: String) extends Exception(s"$message at position $position")
