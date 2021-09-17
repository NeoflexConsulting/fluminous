package org.fluminous.jq.filter.pattern.dsl

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.pattern.PatternCase
import org.fluminous.jq.filter.pattern.dsl.MatchFailure.StackIsNotEnough
import org.fluminous.jq.{ Description, Expression }
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait Matcher[Captured <: HList] {

  def ifValidReplaceBy(builder: Captured => (Int => Expression)): PatternCase = {
    PatternCase(
      size,
      input => stackMatches(input).map { case (position, stack, captured) => builder(captured)(position) +: stack }
    )
  }

  def ->:[E <: Expression](left: CapturedMatcher[E]): CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]] =
    new CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]](left, this)

  def ->:[E <: Expression](left: IsMatcher[E]): CompositeIsMatcher[Captured, E, IsMatcher[E]] =
    new CompositeIsMatcher[Captured, E, IsMatcher[E]](left, this)

  def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, (Int, List[Expression], Captured)]
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
  ): Validated[MatchFailure, (Int, List[Expression], shapeless.::[E, RightCaptured])] = {
    val leftResult = left.stackMatches(input)
    val rightResult =
      NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Invalid(StackIsNotEnough(size)))
    (leftResult, rightResult) match {
      case (Invalid(failure), Valid(_)) =>
        Invalid(failure.copy(firstMismatchPositionFromEnd = size, overallMismatchesQty = MismatchNumber(1)))
      case (Valid((_, _, capturedL)), Valid((position, stack, capturedR))) =>
        Valid((position, stack, shapeless.::(capturedL.head, capturedR)))
      case (Valid(_), Invalid(failure)) =>
        if (failure.overallMismatchesQty == CompleteMismatch) {
          Invalid(failure.copy(overallMismatchesQty = MismatchNumber(right.size)))
        } else {
          Invalid(failure)
        }
      case (Invalid(_), Invalid(failure)) =>
        Invalid(failure.copy(overallMismatchesQty = failure.overallMismatchesQty + MismatchNumber(1)))
    }
  }
}

final class CompositeIsMatcher[RightCaptured <: HList, E <: Expression, M <: IsMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[M, RightCaptured, RightCaptured](left, right) {

  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Validated[MatchFailure, (Int, List[Expression], RightCaptured)] = {
    val leftResult = left.stackMatches(input)
    val rightResult =
      NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Invalid(StackIsNotEnough(size)))
    (leftResult, rightResult) match {
      case (Invalid(failure), Valid(_)) =>
        Invalid(failure.copy(firstMismatchPositionFromEnd = size, overallMismatchesQty = MismatchNumber(1)))
      case (Valid(_), Valid(captured)) =>
        Valid(captured)
      case (Valid(_), Invalid(failure)) =>
        if (failure.overallMismatchesQty == CompleteMismatch) {
          Invalid(failure.copy(overallMismatchesQty = MismatchNumber(right.size)))
        } else {
          Invalid(failure)
        }
      case (Invalid(_), Invalid(failure)) =>
        Invalid(failure.copy(overallMismatchesQty = failure.overallMismatchesQty + MismatchNumber(1)))
    }
  }
}

final class IsMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean) extends BasicMatcher[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, (Int, List[Expression], HNil)] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) =>
        Valid((head.position, tail, HNil))
      case NonEmptyList(head, _) =>
        Invalid(MatchFailure(size, head.description, description, CompleteMismatch))
    }
  }
}

final class CapturedMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Validated[MatchFailure, (Int, List[Expression], shapeless.::[E, HNil])] = {
    input match {
      case NonEmptyList(head, tail) if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        Valid((head.position, tail, head.asInstanceOf[E] :: HNil))
      case NonEmptyList(head, _) =>
        Invalid(MatchFailure(size, head.description, description, CompleteMismatch))
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
