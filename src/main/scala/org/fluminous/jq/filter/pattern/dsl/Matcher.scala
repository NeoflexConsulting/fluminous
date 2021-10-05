package org.fluminous.jq.filter.pattern.dsl

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.pattern.{ MatcherInput, MatcherOutput, PatternCase }
import org.fluminous.jq.{ Expression, ParserException, Tokenizer }
import shapeless.{ HList, HNil }
import scala.reflect.ClassTag

case class MatchingResult[+Failure <: MatchFailure, Captured <: HList](
  tokenizer: Tokenizer,
  result: Validated[Failure, MatchSuccess[Captured]])

sealed trait Matcher[+Failure <: MatchFailure, Captured <: HList] {
  def ifValidReplaceBy(builder: Captured => (Int => Expression)): PatternCase = {
    PatternCase(
      size,
      input =>
        for {
          matchingResult <- stackMatches(input)
        } yield {
          val matcherResult = matchingResult.result.map { matchSuccess =>
            builder(matchSuccess.capturedVariables)(matchSuccess.patternStartPosition) +: matchSuccess.bottomStack
          }
          MatcherOutput(matchingResult.tokenizer, matcherResult)
        }
    )
  }

  def stackMatches(input: MatcherInput): Either[ParserException, MatchingResult[Failure, Captured]]
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

  override def stackMatches(input: MatcherInput): Either[ParserException, MatchingResult[MatchFailure, Captured]] = {
    for {
      leftResult <- checkLeft(input)
      rightMatcherInput = NonEmptyList
        .fromList(input.stack.tail)
        .map(stack => MatcherInput(leftResult.tokenizer, stack))
      rightResult <- rightMatcherInput
                      .map(right.stackMatches)
                      .getOrElse(
                        Right[ParserException, MatchingResult[MatchFailure, RightCaptured]](
                          MatchingResult(leftResult.tokenizer, Invalid(StackIsNotEnough))
                        )
                      )
    } yield {
      (leftResult.result, rightResult.result) match {
        case (Invalid(failure), Valid(success)) =>
          MatchingResult(
            rightResult.tokenizer,
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
            rightResult.tokenizer,
            Valid(
              r.copy(
                capturedVariables = modifyCaptured(capturedL, capturedR)
              )
            )
          )
        case (_, Invalid(StackIsNotEnough)) =>
          MatchingResult(rightResult.tokenizer, Invalid(StackIsNotEnough))
        case (Valid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
          MatchingResult(rightResult.tokenizer, Invalid(p))
        case (Invalid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
          MatchingResult(rightResult.tokenizer, Invalid(p.copy(overallMismatchesQty = p.overallMismatchesQty + 1)))
      }
    }
  }

  protected def checkLeft(
    input: MatcherInput
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, LeftCaptured]] = {
    left.stackMatches(input)
  }

  protected def modifyCaptured(left: LeftCaptured, right: RightCaptured): Captured

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
}

final class CompositeTestMatcher[RightCaptured <: HList, E <: Expression, M <: TestMatcher[E]](
  left: M,
  right: Matcher[MatchFailure, RightCaptured])
    extends CompositeMatcher[E, HNil, RightCaptured, M, RightCaptured](left, right)
    with MatcherOps[MatchFailure, RightCaptured] {
  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured = r
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

  override protected def checkLeft(
    input: MatcherInput
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, HNil]] = {
    left.lookupMatches(input, lookupIndex)
  }

  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured = r
}

final class TestMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, HNil]
    with MatcherOps[PositionedMatchFailure, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: MatcherInput
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, HNil]] = {
    input match {
      case MatcherInput(tokenizer, NonEmptyList(head: E, tail)) if clazz.isInstance(head) && condition(head) =>
        Right(MatchingResult(tokenizer, Valid(MatchSuccess(head.position, tail, HNil))))
      case MatcherInput(tokenizer, NonEmptyList(head, _)) =>
        Right(
          MatchingResult(
            tokenizer,
            Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
          )
        )
    }
  }
}

final class LookupMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, HNil]
    with LookupMatcherOps[PositionedMatchFailure, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass

  def lookupMatches(
    input: MatcherInput,
    lookupIndex: Int
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, HNil]] = {
    input.tokenizer.lookupToken(lookupIndex).map {
      case (tokenizer, Some(token: E)) if clazz.isInstance(token) && condition(token) =>
        MatchingResult(tokenizer, Valid(MatchSuccess(token.position, input.stack.toList, HNil)))
      case (tokenizer, Some(token)) =>
        MatchingResult(
          tokenizer,
          Invalid(PositionedMatchFailure(token.position, token.position, token.description, 1))
        )
      case (tokenizer, None) =>
        MatchingResult(
          tokenizer,
          Invalid(
            PositionedMatchFailure(
              input.stack.head.position,
              input.stack.head.position,
              input.stack.head.description,
              1
            )
          )
        )
    }
  }

  override def stackMatches(
    input: MatcherInput
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, HNil]] = lookupMatches(input, 0)
}

final class CapturedMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]]
    with MatcherOps[PositionedMatchFailure, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: MatcherInput
  ): Either[ParserException, MatchingResult[PositionedMatchFailure, shapeless.::[E, HNil]]] = {
    input match {
      case MatcherInput(tokenizer, NonEmptyList(head, tail))
          if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        Right(MatchingResult(tokenizer, Valid(MatchSuccess(head.position, tail, head.asInstanceOf[E] :: HNil))))
      case MatcherInput(tokenizer, NonEmptyList(head, _)) =>
        Right(
          MatchingResult(
            tokenizer,
            Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
          )
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
