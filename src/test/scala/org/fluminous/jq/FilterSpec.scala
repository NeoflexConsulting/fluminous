package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import io.circe.parser._

class FilterSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {
  "Filters" should {
    "select from json" in {
      checkFilter(
        ".",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}"""
      )

      checkFilter(
        ".foo",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        """{"bar":{"baz":25,"sd":"hello"}}"""
      )

      checkFilter(
        ".foo.bar",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        """{"baz":25,"sd":"hello"}"""
      )

      checkFilter(
        ".foo.bb",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        None
      )

      checkFilter(
        ".foo.bar|.baz",
        """{"foo":{"bar":{"baz":25,"sd":"hello"}},"d":14.0}""",
        """25"""
      )
    }
    "select into Json template" in {
      checkFilter(
        "[.foo, .bar, .baz]",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}, "baz":"hello"}""",
        """[25,{"baz":25,"sd":"hello"},"hello"]"""
      )
      checkFilter(
        "[.foo, .bar, .baz]",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """[25,{"baz":25,"sd":"hello"}]"""
      )

      checkFilter(
        "[.foo, .bar, 44]",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """[25,{"baz":25,"sd":"hello"},44]"""
      )
      checkFilter(
        """[52, "hello", 23.4]""",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """[52, "hello", 23.4]"""
      )

      checkFilter(
        """{"a": 42, "b": 17.4}""",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """{"a": 42, "b": 17.4}"""
      )

      checkFilter(
        """{"a": 42, "b": .bar}""",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """{"a": 42, "b": {"baz":25,"sd":"hello"}}"""
      )

      checkFilter(
        """{foo, bar}""",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}""",
        """{"foo":25, "bar": {"baz":25,"sd":"hello"}}"""
      )
    }
  }

  private def checkFilter(filter: String, input: String, output: String): Unit =
    checkFilter(filter, input, Option(output))
  private def checkFilter(filter: String, input: String, output: Option[String]): Unit = {
    val json = parse(input).right.get
    object JqParser extends Parser
    JqParser.parse(filter).right.get.transform(json) should be(output.map(parse(_).right.get))
  }
}
