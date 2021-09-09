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
      parse(".") should be(Right(Root))
      parse(".foo") should be(Right(Selector(Seq("foo"))))
      parse(".foo.bar") should be(Right(Selector(Seq("foo", "bar"))))
      parse(".foo|.bar") should be(Right(Selector(Seq("foo", "bar"))))
      parse(".foo|.bar|.baz") should be(Right(Selector(Seq("foo", "bar", "baz"))))
      parse("""."foo$"""") should be(Right(Selector(Seq("foo$"))))
    }
    "parse JSON templates" in {
      parse("[.foo, .bar, .baz]") should be(
        Right(
          JsonArrayTemplate(Seq(Right(Selector(Seq("foo"))), Right(Selector(Seq("bar"))), Right(Selector(Seq("baz")))))
        )
      )

      parse("""[52, "hello", 23.4]""") should be(
        Right(
          JsonArrayTemplate(
            Seq(Left(Json.fromInt(52)), Left(Json.fromString("hello")), Left(Json.fromBigDecimal(BigDecimal("23.4"))))
          )
        )
      )
      parse("""{"a": 42, "b": 17.4}""") should be(
        Right(
          JsonObjectTemplate(Map("a" -> Left(Json.fromInt(42)), "b" -> Left(Json.fromBigDecimal(BigDecimal("17.4")))))
        )
      )

      parse("""{"a": 42, "b": .foo}""") should be(
        Right(
          JsonObjectTemplate(Map("a" -> Left(Json.fromInt(42)), "b" -> Right(Selector(Seq("foo")))))
        )
      )

      parse("""{a: 42, b: 17.4}""") should be(
        Right(
          JsonObjectTemplate(Map("a" -> Left(Json.fromInt(42)), "b" -> Left(Json.fromBigDecimal(BigDecimal("17.4")))))
        )
      )

      parse("""{user, title}""") should be(
        Right(
          JsonObjectTemplate(Map("user" -> Right(Selector(Seq("user"))), "title" -> Right(Selector(Seq("title")))))
        )
      )
      parse(""" {customer:.}""") should be(
        Right(JsonObjectTemplate(Map("customer" -> Right(Root))))
      )

      parse("""{customer: {id : .customerId, name, age}}""") should be(
        Right(
          JsonObjectTemplate(
            Map(
              "customer" -> Right(
                JsonObjectTemplate(
                  Map(
                    "id"   -> Right(Selector(Seq("customerId"))),
                    "name" -> Right(Selector(Seq("name"))),
                    "age"  -> Right(Selector(Seq("age")))
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
