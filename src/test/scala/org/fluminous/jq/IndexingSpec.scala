package org.fluminous.jq

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class IndexingSpec extends AnyWordSpecLike with Matchers with BeforeAndAfterAll with TestFunctions {
  "Filters" should {
    "Evaluate expressions with indexes" in {
      checkFilter(
        """.["foo"]""",
        """{"foo": 42}""",
        """42"""
      )
      checkFilter(
        """.["foo"]""",
        """["foo", 42]""",
        Left(EvaluationException(1, "Trying to read field foo from json of type Array"))
      )
      checkFilter(
        """.["foo"]?""",
        """["foo", 42]""",
        """null"""
      )
      checkFilter(
        """.foo?""",
        """{"foo": 42}""",
        """42"""
      )
      checkFilter(
        """.foo?""",
        """["foo", 42]""",
        """null"""
      )
      checkFilter(
        """.["foo"]?""",
        """{"foo": 42}""",
        """42"""
      )
      checkFilter(
        """[.foo?]""",
        """[1,2]""",
        """[]"""
      )
      checkFilter(
        """.[0]""",
        """[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]""",
        """{"name":"JSON", "good":true}"""
      )
      checkFilter(
        """.[2]""",
        """[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]""",
        """null"""
      )
      checkFilter(
        """.[-2]""",
        """[1,2,3]""",
        """2"""
      )
      checkFilter(
        """.[2:4]""",
        """["a","b","c","d","e"]""",
        """["c", "d"]"""
      )
      checkFilter(
        """.[2:4]""",
        """"abcdefghi"""",
        """"cd""""
      )
      checkFilter(
        """.[:3]""",
        """["a","b","c","d","e"]""",
        """["a", "b", "c"]"""
      )
      checkFilter(
        """.[-2:]""",
        """["a","b","c","d","e"]""",
        """["d", "e"]"""
      )
    }
  }
}