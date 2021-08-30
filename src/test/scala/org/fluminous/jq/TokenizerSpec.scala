package org.fluminous.jq
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class TokenizerSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll {

  "Tokenizer" should {
    "split into tokens" in {
      Tokenizer(".").allTokens should be(Right(Seq(Root)))
    }
    "fail in" in {}
  }
}
