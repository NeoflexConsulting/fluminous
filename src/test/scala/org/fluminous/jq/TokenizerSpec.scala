package org.fluminous.jq
import org.fluminous.jq.tokens.symbolic.{
  Colon,
  Comma,
  LeftFigureBracket,
  LeftSquareBracket,
  Minus,
  RightFigureBracket,
  RightSquareBracket,
  Root,
  VerticalSlash
}
import org.fluminous.jq.tokens.{ DecimalNumber, Identifier, NaturalNumber, RawString, RecursiveDescent, Tokenizer }
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class TokenizerSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  "Tokenizer" should {
    "split into tokens" in {
      Tokenizer(".").allTokens should be(Right(Seq(Root(1))))
      tokens.Tokenizer(" .").allTokens should be(Right(Seq(Root(2))))
      tokens.Tokenizer(". ").allTokens should be(Right(Seq(Root(1))))
      tokens.Tokenizer(" . ").allTokens should be(Right(Seq(Root(2))))
      tokens.Tokenizer("  .  ").allTokens should be(Right(Seq(Root(3))))
      tokens.Tokenizer(".foo").allTokens should be(Right(Seq(Root(1), Identifier(2, "foo"))))
      tokens.Tokenizer(". foo").allTokens should be(Right(Seq(Root(1), Identifier(3, "foo"))))
      tokens.Tokenizer(". foo ").allTokens should be(Right(Seq(Root(1), Identifier(3, "foo"))))
      tokens.Tokenizer(" . foo ").allTokens should be(Right(Seq(Root(2), Identifier(4, "foo"))))
      tokens.Tokenizer(".foo.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), Root(5), Identifier(6, "bar")))
      )
      tokens.Tokenizer(".foo|.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(5), Root(6), Identifier(7, "bar")))
      )
      tokens.Tokenizer(".foo |.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(6), Root(7), Identifier(8, "bar")))
      )
      tokens.Tokenizer(".foo | .bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(6), Root(8), Identifier(9, "bar")))
      )
      tokens.Tokenizer(".foo| .bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(5), Root(7), Identifier(8, "bar")))
      )
      tokens.Tokenizer("""."foo$"""").allTokens should be(Right(Seq(Root(1), RawString(2, "foo$"))))
      tokens.Tokenizer(""".["foo"]""").allTokens should be(
        Right(Seq(Root(1), LeftSquareBracket(2), RawString(3, "foo"), RightSquareBracket(8)))
      )
      tokens.Tokenizer(""".[2] .[1]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            NaturalNumber(3, "2"),
            RightSquareBracket(4),
            Root(6),
            LeftSquareBracket(7),
            NaturalNumber(8, "1"),
            RightSquareBracket(9)
          )
        )
      )

      tokens.Tokenizer(""".[-2] .[-1]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            Minus(3),
            NaturalNumber(4, "2"),
            RightSquareBracket(5),
            Root(7),
            LeftSquareBracket(8),
            Minus(9),
            NaturalNumber(10, "1"),
            RightSquareBracket(11)
          )
        )
      )

      tokens.Tokenizer(""".[10:15]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            NaturalNumber(3, "10"),
            Colon(5),
            NaturalNumber(6, "15"),
            RightSquareBracket(8)
          )
        )
      )

      tokens.Tokenizer(""".[]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            RightSquareBracket(3)
          )
        )
      )

      tokens.Tokenizer(""".foo, .bar""").allTokens should be(
        Right(
          Seq(
            Root(1),
            Identifier(2, "foo"),
            Comma(5),
            Root(7),
            Identifier(8, "bar")
          )
        )
      )

      tokens.Tokenizer("""[.foo, .bar, .baz]""").allTokens should be(
        Right(
          Seq(
            LeftSquareBracket(1),
            Root(2),
            Identifier(3, "foo"),
            Comma(6),
            Root(8),
            Identifier(9, "bar"),
            Comma(12),
            Root(14),
            Identifier(15, "baz"),
            RightSquareBracket(18)
          )
        )
      )

      tokens.Tokenizer("""{"a": 42, "b": 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket(1),
            RawString(2, "a"),
            Colon(5),
            NaturalNumber(7, "42"),
            Comma(9),
            RawString(11, "b"),
            Colon(14),
            DecimalNumber(16, "17.5"),
            RightFigureBracket(20)
          )
        )
      )

      tokens.Tokenizer("""{a: 42, b: 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket(1),
            Identifier(2, "a"),
            Colon(3),
            NaturalNumber(5, "42"),
            Comma(7),
            Identifier(9, "b"),
            Colon(10),
            DecimalNumber(12, "17.5"),
            RightFigureBracket(16)
          )
        )
      )
      tokens.Tokenizer("""..|.a""").allTokens should be(
        Right(
          Seq(
            RecursiveDescent(1),
            VerticalSlash(3),
            Root(4),
            Identifier(5, "a")
          )
        )
      )

      tokens.Tokenizer("""..""").allTokens should be(
        Right(
          Seq(
            RecursiveDescent(1)
          )
        )
      )

    }
    "fail in" in {}
  }
}
