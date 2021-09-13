package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ErrorSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Filters" should {
    "select from json" in {
      parse("{customerId, body : {customer: {id : customerId, name, age}} }").left.foreach(e => println(e.toString))
    }
  }
}
