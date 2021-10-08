package org.fluminous.jq

import org.fluminous.jq.filter.json.obj.JsonObject
import org.fluminous.jq.filter.json.array.JsonArray
import org.fluminous.jq.filter.pipe.Pipe
import org.fluminous.jq.filter.selector.{ IdentitySelector, Selector }
import org.fluminous.jq.tokens.{ DecimalNumber, NaturalNumber, RawString }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Parser" should {
    "parse selectors" in {
      parse("25") should be(Right(NaturalNumber(1, "25")))
      parse(".") should be(Right(IdentitySelector(1)))
      parse(".foo") should be(Right(Selector(1, "foo")))
      parse(".foo.bar") should be(Right(Pipe(1, List(Selector(1, "foo"), Selector(5, "bar")))))
      parse(".foo|.bar") should be(Right(Pipe(1, List(Selector(1, "foo"), Selector(6, "bar")))))
      parse(".foo|.bar|.baz") should be(
        Right(Pipe(1, List(Selector(1, "foo"), Selector(6, "bar"), Selector(11, "baz"))))
      )
      parse("""."foo$"""") should be(Right(Selector(1, "foo$")))
    }
    "parse JSON templates" in {
      parse("[.foo, .bar, .baz]") should be(
        Right(
          JsonArray(
            1,
            List(Selector(2, "foo"), Selector(8, "bar"), Selector(14, "baz"))
          )
        )
      )

      parse("""[52, "hello", 23.4]""") should be(
        Right(
          JsonArray(
            1,
            List(NaturalNumber(2, "52"), RawString(6, "hello"), DecimalNumber(15, "23.4"))
          )
        )
      )
      parse("""{"a": 42, "b": 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map("a" -> NaturalNumber(7, "42"), "b" -> DecimalNumber(16, "17.4"))
          )
        )
      )

      parse("""{"a": 42, "b": .foo}""") should be(
        Right(
          JsonObject(1, Map("a" -> NaturalNumber(7, "42"), "b" -> Selector(16, "foo")))
        )
      )

      parse("""{a: 42, b: 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map("a" -> NaturalNumber(5, "42"), "b" -> DecimalNumber(12, "17.4"))
          )
        )
      )

      parse("""{user, title}""") should be(
        Right(
          JsonObject(
            1,
            Map("user" -> Selector(2, "user"), "title" -> Selector(8, "title"))
          )
        )
      )
      parse(""" {customer:.}""") should be(
        Right(JsonObject(2, Map("customer" -> IdentitySelector(12))))
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
                    "id"   -> Selector(18, "customerId"),
                    "name" -> Selector(31, "name"),
                    "age"  -> Selector(37, "age")
                  )
                )
            )
          )
        )
      )
    }
  }
}
