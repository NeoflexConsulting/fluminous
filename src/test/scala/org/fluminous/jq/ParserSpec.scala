package org.fluminous.jq

import io.circe.Json
import org.fluminous.jq.filter.Selector
import org.fluminous.jq.filter.json.{JsonArray, JsonObject}
import org.fluminous.jq.tokens.{DecimalNumber, IntegerNumber, RawString, Root}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Parser" should {
    "parse selectors" in {
      parse("25") should be(Right(IntegerNumber(1, "25")))
      parse(".") should be(Right(Root(1)))
      parse(".foo") should be(Right(Selector(1, Seq("foo"))))
      parse(".foo.bar") should be(Right(Selector(1, Seq("foo", "bar"))))
      parse(".foo|.bar") should be(Right(Selector(1, Seq("foo", "bar"))))
      parse(".foo|.bar|.baz") should be(Right(Selector(1, Seq("foo", "bar", "baz"))))
      parse("""."foo$"""") should be(Right(Selector(1, Seq("foo$"))))
    }
    "parse JSON templates" in {
      parse("[.foo, .bar, .baz]") should be(
        Right(
          JsonArray(
            1,
            List(Selector(2, List("foo")), Selector(8, List("bar")), Selector(14, List("baz")))
          )
        )
      )

      parse("""[52, "hello", 23.4]""") should be(
        Right(
          JsonArray(
            1,
            List(IntegerNumber(2, "52"), RawString(6, "hello"), DecimalNumber(15, "23.4"))
          )
        )
      )
      parse("""{"a": 42, "b": 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map("a" -> IntegerNumber(7, "42"), "b" -> DecimalNumber(16, "17.4"))
          )
        )
      )

      parse("""{"a": 42, "b": .foo}""") should be(
        Right(
          JsonObject(1, Map("a" -> IntegerNumber(7, "42"), "b" -> Selector(16, Seq("foo"))))
        )
      )

      parse("""{a: 42, b: 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map("a" -> IntegerNumber(5, "42"), "b" -> DecimalNumber(12, "17.4"))
          )
        )
      )

      parse("""{user, title}""") should be(
        Right(
          JsonObject(
            1,
            Map("user" -> Selector(2, Seq("user")), "title" -> Selector(8, Seq("title")))
          )
        )
      )
      parse(""" {customer:.}""") should be(
        Right(JsonObject(2, Map("customer" -> Root(12))))
      )

      parse("""{customer: {id : .customerId, name, age}}""") should be(
        Right(
          JsonObject(
            1,
            Map(
              "customer" ->
                JsonObject(
                  12,
                  Map(
                    "id"   -> Selector(18, Seq("customerId")),
                    "name" -> Selector(31, Seq("name")),
                    "age"  -> Selector(37, Seq("age"))
                  )
                )
            )
          )
        )
      )
    }
  }
}
