package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ExpressionsSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with TestFunctions {
  "Filters" should {
    "Evaluate boolean expressions" in {
      checkFilter(
        "true and false ",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        """false"""
      )

      checkFilter(
        ".foo.bar.baz and .d ",
        """{"foo":{"bar":{"baz":true,"sd":"hello"}},"d":true}""",
        """true"""
      )

      checkFilter(
        ".foo.bar.baz and .d and .foo.bar.sd",
        """{"foo":{"bar":{"baz":true,"sd":false}},"d":true}""",
        """false"""
      )

    }
  }
}
