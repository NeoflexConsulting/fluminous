package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class FunctionSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with TestFunctions {
  "Functions" should {
    "be evaluated" in {
      checkFilter(
        ".[] | length",
        """[[1,2], "string", {"a":2}]""",
        List("""2""", """6""", """1""")
      )
      checkFilter(
        "recurse(.foo[])",
        """{"foo":[{"foo": []}, {"foo":[{"foo":[]}]}]}""",
        List(
          """{"foo":[{"foo":[]},{"foo":[{"foo":[]}]}]}""",
          """{"foo":[]}""",
          """{"foo":[{"foo":[]}]}""",
          """{"foo":[]}"""
        )
      )
      checkFilter(
        "recurse",
        """{"a":0,"b":[1]}""",
        List(
          """{"a":0,"b":[1]}""",
          """0""",
          """[1]""",
          """1"""
        )
      )
      checkFilter(
        "recurse(. * .; . < 20)",
        """2""",
        List(
          """2""",
          """4""",
          """16"""
        )
      )
    }
  }
}
