package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ExpressionsSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with TestFunctions {
  "Filters" should {
    "Evaluate arithmetic expressions" in {

      checkFilter(
        ".a + 2*4",
        """{"a": 7}""",
        """15"""
      )
      checkFilter(
        "(.a + 2)*4",
        """{"a": 7}""",
        """36"""
      )
      checkFilter(
        ".a + 1",
        """{"a": 7}""",
        """8"""
      )
      checkFilter(
        ".a + .b",
        """{"a": [1,2], "b": [3,4]}""",
        """[1,2,3,4]"""
      )
      checkFilter(
        ".a + null",
        """{"a": 1}""",
        """1"""
      )
      checkFilter(
        ".a + 1",
        """{}""",
        """1"""
      )
      checkFilter(
        "{a: 1} + {b: 2} + {c: 3} + {a: 42}",
        """null""",
        """{"a": 42, "b": 2, "c": 3}"""
      )
      checkFilter(
        "4 - .a",
        """{"a":3}""",
        """1"""
      )
      checkFilter(
        """. - ["xml", "yaml"]""",
        """["xml", "yaml", "json"]""",
        """["json"]"""
      )
      checkFilter(
        """10 / . * 3""",
        """5""",
        """6"""
      )
      checkFilter(
        """10 % . * 3""",
        """4""",
        """6"""
      )
      checkFilter(
        """. / ", """",
        """"a, b,c,d, e"""",
        """["a","b,c,d","e"]"""
      )
      checkFilter(
        """{"k": {"a": 1, "b": 2}} * {"k": {"a": 0,"c": 3}}""",
        """"null"""",
        """{"k": {"a": 0, "b": 2, "c": 3}}"""
      )
    }

    "Evaluate relational expressions" in {
      checkFilter(
        """. == 1""",
        """1""",
        """true"""
      )
      checkFilter(
        """. == 1""",
        """2""",
        """false"""
      )
      checkFilter(
        """. != 1""",
        """1""",
        """false"""
      )
      checkFilter(
        """.!=1""",
        """2""",
        """true"""
      )
    }

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
