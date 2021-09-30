package org.fluminous.jq.filter.pattern.dsl

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.pattern.{ MatcherInput, MatcherOutput, PatternCase }
import org.fluminous.jq.{ Expression, Tokenizer }
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

case class MatchingResult[+Failure <: MatchFailure, Captured <: HList](
  tokenizer: Tokenizer,
  result: Validated[Failure, MatchSuccess[Captured]])

sealed trait Matcher[+Failure <: MatchFailure, Captured <: HList] {
  def ifValidReplaceBy(builder: Captured => (Int => Expression)): PatternCase = {
    PatternCase(
      size,
      input => {
        val matchingResult = stackMatches(input)
        val matcherResult = matchingResult.result.map { matchSuccess =>
          builder(matchSuccess.capturedVariables)(matchSuccess.patternStartPosition) +: matchSuccess.bottomStack
        }
        MatcherOutput(matchingResult.tokenizer, matcherResult)
      }
    )
  }

  def stackMatches(input: MatcherInput): MatchingResult[Failure, Captured]
  val size: Int
}

trait LookupMatcherOps[Failure <: MatchFailure, Captured <: HList] extends Matcher[Failure, Captured] {
  def ->:[E <: Expression](left: LookupMatcher[E]): CompositeLookupMatcher[Captured, E, LookupMatcher[E]] =
    new CompositeLookupMatcher[Captured, E, LookupMatcher[E]](left, this, 0)
}

trait MatcherOps[Failure <: MatchFailure, Captured <: HList] extends LookupMatcherOps[Failure, Captured] {
  def ->:[E <: Expression](left: TestMatcher[E]): CompositeTestMatcher[Captured, E, TestMatcher[E]] =
    new CompositeTestMatcher[Captured, E, TestMatcher[E]](left, this)
  def ->:[E <: Expression](left: CapturedMatcher[E]): CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]] =
    new CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]](left, this)
}

abstract class BasicMatcher[E <: Expression, Captured <: HList] extends Matcher[PositionedMatchFailure, Captured] {
  override val size: Int = 1
}
abstract class CompositeMatcher[
  E <: Expression,
  LeftCaptured <: HList,
  RightCaptured <: HList,
  M <: BasicMatcher[E, LeftCaptured],
  Captured <: HList
](
  left: M,
  right: Matcher[MatchFailure, RightCaptured])
    extends Matcher[MatchFailure, Captured] {

  override def stackMatches(input: MatcherInput): MatchingResult[MatchFailure, Captured] = {
    val MatchingResult(leftTokenizer, leftResult) = left.stackMatches(input)
    val rightMatcherInput                         = NonEmptyList.fromList(input.stack.tail).map(stack => MatcherInput(leftTokenizer, stack))
    val MatchingResult(rightTokenizer, rightResult) =
      rightMatcherInput.map(right.stackMatches).getOrElse(MatchingResult(leftTokenizer, Invalid(StackIsNotEnough)))
    (leftResult, rightResult) match {
      case (Invalid(failure), Valid(success)) =>
        MatchingResult(
          rightTokenizer,
          Invalid(
            failure.copy(
              patternStartPosition = success.patternStartPosition,
              failurePosition = input.stack.head.position,
              overallMismatchesQty = 1
            )
          )
        )
      case (Valid(MatchSuccess(_, _, capturedL)), Valid(r @ MatchSuccess(_, _, capturedR))) =>
        MatchingResult(
          rightTokenizer,
          Valid(
            r.copy(
              capturedVariables = modifyCaptured(capturedL, capturedR)
            )
          )
        )
      case (_, Invalid(StackIsNotEnough)) =>
        MatchingResult(rightTokenizer, Invalid(StackIsNotEnough))
      case (Valid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
        MatchingResult(rightTokenizer, Invalid(p))
      case (Invalid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
        MatchingResult(rightTokenizer, Invalid(p.copy(overallMismatchesQty = p.overallMismatchesQty + 1)))
    }
  }

  protected def modifyCaptured(left: LeftCaptured, right: RightCaptured): Captured
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression]

  override val size: Int = right.size + 1
}

final class CompositeCaptureMatcher[RightCaptured <: HList, E <: Expression, M <: CapturedMatcher[E]](
  left: M,
  right: Matcher[MatchFailure, RightCaptured])
    extends CompositeMatcher[E, shapeless.::[E, HNil], RightCaptured, M, shapeless.::[E, RightCaptured]](left, right)
    with MatcherOps[MatchFailure, shapeless.::[E, RightCaptured]] {
  protected override def modifyCaptured(l: shapeless.::[E, HNil], r: RightCaptured): shapeless.::[E, RightCaptured] = {
    shapeless.::(l.head, r)
  }
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = topStack
}

final class CompositeTestMatcher[RightCaptured <: HList, E <: Expression, M <: TestMatcher[E]](
  left: M,
  right: Matcher[MatchFailure, RightCaptured])
    extends CompositeMatcher[E, HNil, RightCaptured, M, RightCaptured](left, right)
    with MatcherOps[MatchFailure, RightCaptured] {
  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured           = r
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = topStack
}

final class CompositeLookupMatcher[RightCaptured <: HList, E <: Expression, M <: LookupMatcher[E]](
  left: M,
  right: Matcher[MatchFailure, RightCaptured],
  lookupIndex: Int)
    extends CompositeMatcher[E, HNil, RightCaptured, M, RightCaptured](left, right)
    with LookupMatcherOps[MatchFailure, RightCaptured] {

  override def ->:[E <: Expression](
    left: LookupMatcher[E]
  ): CompositeLookupMatcher[RightCaptured, E, LookupMatcher[E]] =
    new CompositeLookupMatcher[RightCaptured, E, LookupMatcher[E]](left, this, lookupIndex + 1)

  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured           = r
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = e +: topStack

}

final class TestMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, HNil]
    with MatcherOps[PositionedMatchFailure, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: MatcherInput): MatchingResult[PositionedMatchFailure, HNil] = {
    input match {
      case MatcherInput(tokenizer, NonEmptyList(head: E, tail)) if clazz.isInstance(head) && condition(head) =>
        MatchingResult(tokenizer, Valid(MatchSuccess(head.position, tail, HNil)))
      case MatcherInput(tokenizer, NonEmptyList(head, _)) =>
        MatchingResult(
          tokenizer,
          Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
        )
    }
  }
}

final class LookupMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, HNil]
    with LookupMatcherOps[PositionedMatchFailure, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: MatcherInput): MatchingResult[PositionedMatchFailure, HNil] = {
    ???
    /*   input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) =>
        Valid(MatchSuccess(head.position, tail, HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
    }
  }*/
  }
}

final class CapturedMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]]
    with MatcherOps[PositionedMatchFailure, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: MatcherInput): MatchingResult[PositionedMatchFailure, shapeless.::[E, HNil]] = {
    input match {
      case MatcherInput(tokenizer, NonEmptyList(head, tail))
          if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        MatchingResult(tokenizer, Valid(MatchSuccess(head.position, tail, head.asInstanceOf[E] :: HNil)))
      case MatcherInput(tokenizer, NonEmptyList(head, _)) =>
        MatchingResult(
          tokenizer,
          Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
        )
    }
  }
}

object Matcher {

  def lookup[E <: Expression: ClassTag]: LookupMatcher[E] = lookupThat[E](_ => true)

  def lookupThat[E <: Expression: ClassTag](condition: E => Boolean): LookupMatcher[E] =
    new LookupMatcher[E](condition)

  def test[E <: Expression: ClassTag]: TestMatcher[E] = testThat[E](_ => true)

  def testThat[E <: Expression: ClassTag](condition: E => Boolean): TestMatcher[E] =
    new TestMatcher[E](condition)

  def capture[E <: Expression: ClassTag]: CapturedMatcher[E] = captureIf[E](_ => true)

  def captureIf[E <: Expression: ClassTag](condition: E => Boolean): CapturedMatcher[E] =
    new CapturedMatcher[E](condition)
}
