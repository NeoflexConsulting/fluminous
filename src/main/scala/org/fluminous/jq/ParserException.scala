package org.fluminous.jq

case class ParserException(position: Int, message: String) extends Exception
