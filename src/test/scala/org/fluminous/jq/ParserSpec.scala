package org.fluminous.jq

import org.fluminous.jq.filter.Selector
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
  }
}
