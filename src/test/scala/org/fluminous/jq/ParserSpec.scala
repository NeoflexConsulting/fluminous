package org.fluminous.jq

import io.circe.Json
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonObjectTemplate, Selector }
import org.fluminous.jq.tokens.Root
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Parser" should {
    "parse selectors" in {
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
          JsonArrayTemplate(
            1,
            Seq(Right(Selector(2, Seq("foo"))), Right(Selector(8, Seq("bar"))), Right(Selector(14, Seq("baz"))))
          )
        )
      )

      parse("""[52, "hello", 23.4]""") should be(
        Right(
          JsonArrayTemplate(
            1,
            Seq(Left(Json.fromInt(52)), Left(Json.fromString("hello")), Left(Json.fromBigDecimal(BigDecimal("23.4"))))
          )
        )
      )
      parse("""{"a": 42, "b": 17.4}""") should be(
        Right(
          JsonObjectTemplate(
            1,
            Map("a" -> Left(Json.fromInt(42)), "b" -> Left(Json.fromBigDecimal(BigDecimal("17.4"))))
          )
        )
      )

      parse("""{"a": 42, "b": .foo}""") should be(
        Right(
          JsonObjectTemplate(1, Map("a" -> Left(Json.fromInt(42)), "b" -> Right(Selector(16, Seq("foo")))))
        )
      )

      parse("""{a: 42, b: 17.4}""") should be(
        Right(
          JsonObjectTemplate(
            1,
            Map("a" -> Left(Json.fromInt(42)), "b" -> Left(Json.fromBigDecimal(BigDecimal("17.4"))))
          )
        )
      )

      parse("""{user, title}""") should be(
        Right(
          JsonObjectTemplate(
            1,
            Map("user" -> Right(Selector(2, Seq("user"))), "title" -> Right(Selector(8, Seq("title"))))
          )
        )
      )
      parse(""" {customer:.}""") should be(
        Right(JsonObjectTemplate(1, Map("customer" -> Right(Root(11)))))
      )

      parse("""{customer: {id : .customerId, name, age}}""") should be(
        Right(
          JsonObjectTemplate(
            1,
            Map(
              "customer" -> Right(
                JsonObjectTemplate(
                  12,
                  Map(
                    "id"   -> Right(Selector(18, Seq("customerId"))),
                    "name" -> Right(Selector(31, Seq("name"))),
                    "age"  -> Right(Selector(37, Seq("age")))
                  )
                )
              )
            )
          )
        )
      )
    }
  }
}
