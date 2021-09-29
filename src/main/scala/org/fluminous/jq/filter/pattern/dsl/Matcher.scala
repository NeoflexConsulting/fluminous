package org.fluminous.jq.filter.pattern.dsl

import cats.data.Validated.{ Invalid, Valid }
import cats.data.{ NonEmptyList, Validated }
import org.fluminous.jq.filter.pattern.PatternCase
import org.fluminous.jq.Expression
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait Matcher[Captured <: HList] {

  def ifValidReplaceBy(builder: Captured => (Int => Expression)): PatternCase = {
    PatternCase(
      size,
      input =>
        stackMatches(input).map { matchSuccess =>
          builder(matchSuccess.capturedVariables)(matchSuccess.patternStartPosition) +: matchSuccess.bottomStack
        }
    )
  }

  def ->:[E <: Expression](left: CapturedMatcher[E]): CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]] =
    new CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]](left, this)

  def ->:[E <: Expression](left: DropMatcher[E]): CompositeDropMatcher[Captured, E, DropMatcher[E]] =
    new CompositeDropMatcher[Captured, E, DropMatcher[E]](left, this)

  def ->:[E <: Expression](left: TestMatcher[E]): CompositeTestMatcher[Captured, E, TestMatcher[E]] =
    new CompositeTestMatcher[Captured, E, TestMatcher[E]](left, this)

  def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, MatchSuccess[Captured]]
  val size: Int
}

abstract class BasicMatcher[E <: Expression, Captured <: HList] extends Matcher[Captured] {
  override def stackMatches(input: NonEmptyList[Expression]): Validated[PositionedMatchFailure, MatchSuccess[Captured]]
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
  right: Matcher[RightCaptured])
    extends Matcher[Captured] {

  override def stackMatches(input: NonEmptyList[Expression]): Validated[MatchFailure, MatchSuccess[Captured]] = {
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
      case (Valid(MatchSuccess(_, _, _, capturedL)), Valid(r @ MatchSuccess(_, topStack, _, capturedR))) =>
        Valid(
          r.copy(
            capturedVariables = modifyCaptured(capturedL, capturedR),
            topStack = modifyTopStack(input.head, r.topStack)
          )
        )
      case (_, Invalid(StackIsNotEnough)) =>
        Invalid(StackIsNotEnough)
      case (Valid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
        Invalid(p)
      case (Invalid(_), Invalid(p @ PositionedMatchFailure(_, _, _, _))) =>
        Invalid(p.copy(overallMismatchesQty = p.overallMismatchesQty + 1))
    }
  }

  protected def modifyCaptured(left: LeftCaptured, right: RightCaptured): Captured
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression]

  override val size: Int = right.size + 1
}

final class CompositeCaptureMatcher[RightCaptured <: HList, E <: Expression, M <: CapturedMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[E, shapeless.::[E, HNil], RightCaptured, M, shapeless.::[E, RightCaptured]](left, right) {
  protected override def modifyCaptured(l: shapeless.::[E, HNil], r: RightCaptured): shapeless.::[E, RightCaptured] = {
    shapeless.::(l.head, r)
  }
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = topStack
}

final class CompositeDropMatcher[RightCaptured <: HList, E <: Expression, M <: DropMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[E, HNil, RightCaptured, M, RightCaptured](left, right) {
  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured           = r
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = topStack
}

final class CompositeTestMatcher[RightCaptured <: HList, E <: Expression, M <: TestMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[E, HNil, RightCaptured, M, RightCaptured](left, right) {
  protected override def modifyCaptured(l: HNil, r: RightCaptured): RightCaptured           = r
  protected def modifyTopStack(e: Expression, topStack: List[Expression]): List[Expression] = e +: topStack
}

final class DropMatcher[E <: Expression: ClassTag](condition: E => Boolean) extends BasicMatcher[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Validated[PositionedMatchFailure, MatchSuccess[HNil]] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) =>
        Valid(MatchSuccess(head.position, List.empty, tail, HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
    }
  }
}

final class TestMatcher[E <: Expression: ClassTag](condition: E => Boolean) extends BasicMatcher[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Validated[PositionedMatchFailure, MatchSuccess[HNil]] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) =>
        Valid(MatchSuccess(head.position, List(head), tail, HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
    }
  }
}

final class CapturedMatcher[E <: Expression: ClassTag](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Validated[PositionedMatchFailure, MatchSuccess[shapeless.::[E, HNil]]] = {
    input match {
      case NonEmptyList(head, tail) if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        Valid(MatchSuccess(head.position, List.empty, tail, head.asInstanceOf[E] :: HNil))
      case NonEmptyList(head, _) =>
        Invalid(PositionedMatchFailure(head.position, head.position, head.description, 1))
    }
  }
}

object Matcher {

  def test[E <: Expression: ClassTag]: TestMatcher[E] = testIf[E](_ => true)

  def testIf[E <: Expression: ClassTag](condition: E => Boolean): TestMatcher[E] =
    new TestMatcher[E](condition)

  def testAndDrop[E <: Expression: ClassTag]: DropMatcher[E] = testAndDropIf[E](_ => true)

  def testAndDropIf[E <: Expression: ClassTag](condition: E => Boolean): DropMatcher[E] =
    new DropMatcher[E](condition)

  def capture[E <: Expression: ClassTag]: CapturedMatcher[E] = captureIf[E](_ => true)

  def captureIf[E <: Expression: ClassTag](condition: E => Boolean): CapturedMatcher[E] =
    new CapturedMatcher[E](condition)
}
