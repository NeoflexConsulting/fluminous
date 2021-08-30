package org.fluminous.jq
import org.fluminous.jq.tokens.{Colon, Comma, DecimalNumber, Identifier, IntegerNumber, LeftFigureBracket, LeftSquareBracket, Pipe, RawString, RecursiveDescent, RightFigureBracket, RightSquareBracket, Root}
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
      Tokenizer(""".[2] .[1]""").allTokens should be(
        Right(
          Seq(
            Root,
            LeftSquareBracket,
            IntegerNumber("2"),
            RightSquareBracket,
            Root,
            LeftSquareBracket,
            IntegerNumber("1"),
            RightSquareBracket
          )
        )
      )

      Tokenizer(""".[-2] .[-1]""").allTokens should be(
        Right(
          Seq(
            Root,
            LeftSquareBracket,
            IntegerNumber("-2"),
            RightSquareBracket,
            Root,
            LeftSquareBracket,
            IntegerNumber("-1"),
            RightSquareBracket
          )
        )
      )

      Tokenizer(""".[10:15]""").allTokens should be(
        Right(
          Seq(
            Root,
            LeftSquareBracket,
            IntegerNumber("10"),
            Colon,
            IntegerNumber("15"),
            RightSquareBracket
          )
        )
      )

      Tokenizer(""".[]""").allTokens should be(
        Right(
          Seq(
            Root,
            LeftSquareBracket,
            RightSquareBracket
          )
        )
      )

      Tokenizer(""".foo, .bar""").allTokens should be(
        Right(
          Seq(
            Root,
            Identifier("foo"),
            Comma,
            Root,
            Identifier("bar")
          )
        )
      )

      Tokenizer("""[.foo, .bar, .baz]""").allTokens should be(
        Right(
          Seq(
            LeftSquareBracket,
            Root,
            Identifier("foo"),
            Comma,
            Root,
            Identifier("bar"),
            Comma,
            Root,
            Identifier("baz"),
            RightSquareBracket
          )
        )
      )

      Tokenizer("""{"a": 42, "b": 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket,
            RawString("a"),
            Colon,
            IntegerNumber("42"),
            Comma,
            RawString("b"),
            Colon,
            DecimalNumber("17.5"),
            RightFigureBracket
          )
        )
      )

      Tokenizer("""{a: 42, b: 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket,
            Identifier("a"),
            Colon,
            IntegerNumber("42"),
            Comma,
            Identifier("b"),
            Colon,
            DecimalNumber("17.5"),
            RightFigureBracket
          )
        )
      )
      Tokenizer("""..|.a""").allTokens should be(
        Right(
          Seq(
            RecursiveDescent,
            Pipe,
            Root,
            Identifier("a")
          )
        )
      )

      Tokenizer("""..""").allTokens should be(
        Right(
          Seq(
            RecursiveDescent
          )
        )
      )

    }
    "fail in" in {}
  }
}
