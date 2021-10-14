package org.fluminous.jq

import io.circe.parser.parse

import org.scalatest.matchers.should.Matchers

trait TestFunctions extends Matchers {

  protected def checkFilter(filter: String, input: String, output: Left[EvaluationException, Nothing]): Unit = {
    val json = parse(input).right.get
    object JqParser extends Parser
    JqParser.parse(filter).right.get.transform(json) should be(output)
  }

  protected def checkFilter(filter: String, input: String, output: String): Unit = {
    val json = parse(input).right.get
    object JqParser extends Parser
    JqParser.parse(filter).right.get.transform(json) should be(Right(List(parse(output).right.get)))
  }
}
