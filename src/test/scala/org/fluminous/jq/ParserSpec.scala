package org.fluminous.jq

import org.fluminous.jq.filter.algebra.IntegerNumber
import org.fluminous.jq.filter.json.obj.JsonObject
import org.fluminous.jq.filter.json.array.JsonArray
import org.fluminous.jq.filter.pipe.Pipe
import org.fluminous.jq.filter.selector.{ IdentitySelector, SelectorByName }
import org.fluminous.jq.filter.sequence.FilterSequence
import org.fluminous.jq.tokens.{ DecimalNumber, RawString }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class ParserSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with Parser {
  "Parser" should {
    "parse selectors" in {
      parse("25") should be(Right(IntegerNumber(1, 25)))
      parse(".") should be(Right(IdentitySelector(1)))
      parse(".foo") should be(Right(SelectorByName(1, "foo")))
      parse(".foo.bar") should be(Right(Pipe(1, List(SelectorByName(1, "foo"), SelectorByName(5, "bar")))))
      parse(".foo|.bar") should be(Right(Pipe(1, List(SelectorByName(1, "foo"), SelectorByName(6, "bar")))))
      parse(".foo|.bar|.baz") should be(
        Right(Pipe(1, List(SelectorByName(1, "foo"), SelectorByName(6, "bar"), SelectorByName(11, "baz"))))
      )
      parse("""."foo$"""") should be(Right(SelectorByName(1, "foo$")))
    }
    "parse JSON templates" in {
      parse("[.foo, .bar, .baz]") should be(
        Right(
          JsonArray(
            1,
              FilterSequence(
                2,
                List(SelectorByName(2, "foo"), SelectorByName(8, "bar"), SelectorByName(14, "baz"))
              )
            )
        )
      )

      parse("""[52, "hello", 23.4]""") should be(
        Right(
          JsonArray(
            1,
           FilterSequence(2, List(IntegerNumber(2, 52), RawString(6, "hello"), DecimalNumber(15, "23.4"))))
        )
      )
      parse("""{"a": 42, "b": 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map(RawString(2, "a") -> IntegerNumber(7, 42), RawString(11, "b") -> DecimalNumber(16, "17.4"))
          )
        )
      )

      parse("""{"a": 42, "b": .foo}""") should be(
        Right(
          JsonObject(1, Map(RawString(2, "a") -> IntegerNumber(7, 42), RawString(11, "b") -> SelectorByName(16, "foo")))
        )
      )

      parse("""{a: 42, b: 17.4}""") should be(
        Right(
          JsonObject(
            1,
            Map(RawString(2, "a") -> IntegerNumber(5, 42), RawString(9, "b") -> DecimalNumber(12, "17.4"))
          )
        )
      )

      parse("""{user, title}""") should be(
        Right(
          JsonObject(
            1,
            Map(
              RawString(2, "user")  -> SelectorByName(2, "user"),
              RawString(8, "title") -> SelectorByName(8, "title")
            )
          )
        )
      )
      parse(""" {customer:.}""") should be(
        Right(JsonObject(2, Map(RawString(3, "customer") -> IdentitySelector(12))))
      )

      parse("""{customer: {id : .customerId, name, age}}""") should be(
        Right(
          JsonObject(
            1,
            Map(
              RawString(2, "customer") ->
                JsonObject(
                  12,
                  Map(
                    RawString(13, "id")   -> SelectorByName(18, "customerId"),
                    RawString(31, "name") -> SelectorByName(31, "name"),
                    RawString(37, "age")  -> SelectorByName(37, "age")
                  )
                )
            )
          )
        )
      )
    }
  }
}
