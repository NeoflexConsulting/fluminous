package org.fluminous.jq.filter.pattern.dsl

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.pattern.PatternCase
import org.fluminous.jq.{ Description, Expression }
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait Matcher[Captured <: HList] {

  def ifValidReplaceBy(builder: Captured => (Int => Expression)): PatternCase = {
    PatternCase(
      size,
      input =>
        stackMatches(input).map { matchSuccess =>
          builder(matchSuccess.capturedVariables)(matchSuccess.patternStartPosition) +: matchSuccess.remainingStack
        }
    )
  }

  def ->:[E <: Expression](left: CapturedMatcher[E]): CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]] =
    new CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]](left, this)

  def ->:[E <: Expression](left: IsMatcher[E]): CompositeIsMatcher[Captured, E, IsMatcher[E]] =
    new CompositeIsMatcher[Captured, E, IsMatcher[E]](left, this)

  def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, MatchSuccess[Captured]]
  val size: Int
}

abstract class BasicMatcher[E <: Expression: Description, Captured <: HList] extends Matcher[Captured] {
  val description: String = implicitly[Description[E]].description
  override val size: Int  = 1
}

abstract class CompositeMatcher[M <: BasicMatcher[_, _], RightCaptured <: HList, Captured <: HList](
  left: M,
  right: Matcher[RightCaptured])
    extends Matcher[Captured] {
  override val size: Int = right.size + 1
}

final class CompositeCaptureMatcher[RightCaptured <: HList, E <: Expression, M <: CapturedMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[M, RightCaptured, shapeless.::[E, RightCaptured]](left, right) {
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Validated[MatchFailure, MatchSuccess[shapeless.::[E, RightCaptured]]] = {
    val leftResult = left.stackMatches(input)
    val rightResult =
      NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Invalid(StackIsNotEnough))
    (leftResult, rightResult) match {
      case (Invalid(failure), Valid(success)) =>
        Invalid(
          failure.copy(
            patternStartPosition = success.patternStartPosition,
            failurePosition = input.head.position,
            overallMismatchesQty = 1
          )
        )
      case (Valid(MatchSuccess(_, _, capturedL)), Valid(r @ MatchSuccess(_, _, capturedR))) =>
        Valid(r.copy(capturedVariables = shapeless.::(capturedL.head, capturedR)))
      case (_, Invalid(StackIsNotEnough)) =>
        Invalid(StackIsNotEnough)
      case (Valid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _, _))) =>
        Invalid(p)
      case (Invalid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _, _))) =>
        Invalid(p.copy(overallMismatchesQty = p.overallMismatchesQty + 1))
    }
  }
}

final class CompositeIsMatcher[RightCaptured <: HList, E <: Expression, M <: IsMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[M, RightCaptured, RightCaptured](left, right) {

  override def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, MatchSuccess[RightCaptured]] = {
    val leftResult = left.stackMatches(input)
    val rightResult =
      NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Invalid(StackIsNotEnough))
    (leftResult, rightResult) match {
      case (Invalid(failure), Valid(success)) =>
        Invalid(
          failure.copy(
            patternStartPosition = success.patternStartPosition,
            failurePosition = input.head.position,
            overallMismatchesQty = 1
          )
        )
      case (Valid(_), Valid(captured)) =>
        Valid(captured)
      case (_, Invalid(StackIsNotEnough)) =>
        Invalid(StackIsNotEnough)
      case (Valid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _, _))) =>
        Invalid(p)
      case (Invalid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _, _))) =>
        Invalid(p.copy(overallMismatchesQty = p.overallMismatchesQty + 1))
    }
  }
}

final class IsMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean) extends BasicMatcher[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Validated[PositionedMatchFailure, MatchSuccess[HNil]] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) =>
        Valid(MatchSuccess(head.position, tail, HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, description, 1))
    }
  }
}

final class CapturedMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Validated[PositionedMatchFailure, MatchSuccess[shapeless.::[E, HNil]]] = {
    input match {
      case NonEmptyList(head, tail) if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        Valid(MatchSuccess(head.position, tail, head.asInstanceOf[E] :: HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, description, 1))
    }
  }
}

object Matcher {
  def check[E <: Expression: ClassTag: Description]: IsMatcher[E] = checkIf[E](_ => true)

  def checkIf[E <: Expression: ClassTag: Description](condition: E => Boolean): IsMatcher[E] =
    new IsMatcher[E](condition)

  def capture[E <: Expression: ClassTag: Description]: CapturedMatcher[E] = captureIf[E](_ => true)

  def captureIf[E <: Expression: ClassTag: Description](condition: E => Boolean): CapturedMatcher[E] =
    new CapturedMatcher[E](condition)
}
