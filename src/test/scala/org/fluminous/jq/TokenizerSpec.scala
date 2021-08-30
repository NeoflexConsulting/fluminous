package org.fluminous.jq
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class TokenizerSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  "Tokenizer" should {
    "split into tokens" in {
      Tokenizer(".").allTokens should be(Right(Seq(Root)))
      Tokenizer(" .").allTokens should be(Right(Seq(Root)))
      Tokenizer(". ").allTokens should be(Right(Seq(Root)))
      Tokenizer(" . ").allTokens should be(Right(Seq(Root)))
      Tokenizer("  .  ").allTokens should be(Right(Seq(Root)))
      Tokenizer(".foo").allTokens should be(Right(Seq(Root, Identifier("foo"))))
      Tokenizer(". foo").allTokens should be(Right(Seq(Root, Identifier("foo"))))
      Tokenizer(". foo ").allTokens should be(Right(Seq(Root, Identifier("foo"))))
      Tokenizer(" . foo ").allTokens should be(Right(Seq(Root, Identifier("foo"))))
      Tokenizer(".foo.bar").allTokens should be(Right(Seq(Root, Identifier("foo"), Root, Identifier("bar"))))
      Tokenizer(".foo|.bar").allTokens should be(Right(Seq(Root, Identifier("foo"), Pipe, Root, Identifier("bar"))))
      Tokenizer(".foo |.bar").allTokens should be(Right(Seq(Root, Identifier("foo"), Pipe, Root, Identifier("bar"))))
      Tokenizer(".foo | .bar").allTokens should be(Right(Seq(Root, Identifier("foo"), Pipe, Root, Identifier("bar"))))
      Tokenizer(".foo| .bar").allTokens should be(Right(Seq(Root, Identifier("foo"), Pipe, Root, Identifier("bar"))))
      Tokenizer("""."foo$"""").allTokens should be(Right(Seq(Root, RawString("foo$"))))
      Tokenizer(""".["foo"]""").allTokens should be(
        Right(Seq(Root, LeftSquareBracket, RawString("foo"), RightSquareBracket))
      )
    }
    "fail in" in {}
  }
}
