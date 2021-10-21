package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ErrorSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Parsing of wrong jq statements" should {
    "fail with error" in {
      parse("{customerId, body : {customer: {id : customerId, name, age}} }") should be(
        Left(ParserException(38, "Error occurred while parsing selector by name: Unexpected customerId"))
      )

      parse("{name, age}}") should be(
        Left(ParserException(12, "Unexpected }"))
      )
    }
  }
}
