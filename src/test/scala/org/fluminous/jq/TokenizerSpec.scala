package org.fluminous.jq
import org.fluminous.jq.tokens.{
  Colon,
  Comma,
  DecimalNumber,
  Identifier,
  IntegerNumber,
  LeftFigureBracket,
  LeftSquareBracket,
  VerticalSlash,
  RawString,
  RecursiveDescent,
  RightFigureBracket,
  RightSquareBracket,
  Root
}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class TokenizerSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  "Tokenizer" should {
    "split into tokens" in {
      Tokenizer(".").allTokens should be(Right(Seq(Root(1))))
      Tokenizer(" .").allTokens should be(Right(Seq(Root(2))))
      Tokenizer(". ").allTokens should be(Right(Seq(Root(1))))
      Tokenizer(" . ").allTokens should be(Right(Seq(Root(2))))
      Tokenizer("  .  ").allTokens should be(Right(Seq(Root(3))))
      Tokenizer(".foo").allTokens should be(Right(Seq(Root(1), Identifier(2, "foo"))))
      Tokenizer(". foo").allTokens should be(Right(Seq(Root(1), Identifier(3, "foo"))))
      Tokenizer(". foo ").allTokens should be(Right(Seq(Root(1), Identifier(3, "foo"))))
      Tokenizer(" . foo ").allTokens should be(Right(Seq(Root(2), Identifier(4, "foo"))))
      Tokenizer(".foo.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), Root(5), Identifier(6, "bar")))
      )
      Tokenizer(".foo|.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(5), Root(6), Identifier(7, "bar")))
      )
      Tokenizer(".foo |.bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(6), Root(7), Identifier(8, "bar")))
      )
      Tokenizer(".foo | .bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(6), Root(8), Identifier(9, "bar")))
      )
      Tokenizer(".foo| .bar").allTokens should be(
        Right(Seq(Root(1), Identifier(2, "foo"), VerticalSlash(5), Root(7), Identifier(8, "bar")))
      )
      Tokenizer("""."foo$"""").allTokens should be(Right(Seq(Root(1), RawString(2, "foo$"))))
      Tokenizer(""".["foo"]""").allTokens should be(
        Right(Seq(Root(1), LeftSquareBracket(2), RawString(3, "foo"), RightSquareBracket(8)))
      )
      Tokenizer(""".[2] .[1]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            IntegerNumber(3, "2"),
            RightSquareBracket(4),
            Root(6),
            LeftSquareBracket(7),
            IntegerNumber(8, "1"),
            RightSquareBracket(9)
          )
        )
      )

      Tokenizer(""".[-2] .[-1]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            IntegerNumber(3, "-2"),
            RightSquareBracket(5),
            Root(7),
            LeftSquareBracket(8),
            IntegerNumber(9, "-1"),
            RightSquareBracket(11)
          )
        )
      )

      Tokenizer(""".[10:15]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            IntegerNumber(3, "10"),
            Colon(5),
            IntegerNumber(6, "15"),
            RightSquareBracket(8)
          )
        )
      )

      Tokenizer(""".[]""").allTokens should be(
        Right(
          Seq(
            Root(1),
            LeftSquareBracket(2),
            RightSquareBracket(3)
          )
        )
      )

      Tokenizer(""".foo, .bar""").allTokens should be(
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

      Tokenizer("""[.foo, .bar, .baz]""").allTokens should be(
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

      Tokenizer("""{"a": 42, "b": 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket(1),
            RawString(2, "a"),
            Colon(5),
            IntegerNumber(7, "42"),
            Comma(9),
            RawString(11, "b"),
            Colon(14),
            DecimalNumber(16, "17.5"),
            RightFigureBracket(20)
          )
        )
      )

      Tokenizer("""{a: 42, b: 17.5}""").allTokens should be(
        Right(
          Seq(
            LeftFigureBracket(1),
            Identifier(2, "a"),
            Colon(3),
            IntegerNumber(5, "42"),
            Comma(7),
            Identifier(9, "b"),
            Colon(10),
            DecimalNumber(12, "17.5"),
            RightFigureBracket(16)
          )
        )
      )
      Tokenizer("""..|.a""").allTokens should be(
        Right(
          Seq(
            RecursiveDescent(1),
            VerticalSlash(3),
            Root(4),
            Identifier(5, "a")
          )
        )
      )

      Tokenizer("""..""").allTokens should be(
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
