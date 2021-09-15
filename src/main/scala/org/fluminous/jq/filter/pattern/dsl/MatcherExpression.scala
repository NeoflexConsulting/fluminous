package org.fluminous.jq.filter.pattern.dsl

import cats.data.NonEmptyList
import org.fluminous.jq.Expression
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait InconsistencyMeasure {
  def +(r: InconsistencyMeasure): InconsistencyMeasure
}

case class InconsistencyNumber(number: Int) extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = {
    r match {
      case InconsistencyNumber(ir) => InconsistencyNumber(number + ir)
      case _                       => Infinite
    }
  }
}

case object Infinite extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = this
}

trait MatcherExpression[Captured <: HList] {
  def ifValidReplaceBy(
    builder: Captured => List[Expression]
  ): NonEmptyList[Expression] => Either[InconsistencyMeasure, List[Expression]] = { input =>
    stackMatches(input).map { case (stack, captured) => builder(captured) ++ stack }
  }
  def ->:[E <: Expression](
    left: CapturedMatcherExpression[E]
  ): CompositeCapturedMatcherExpression[Captured, E, CapturedMatcherExpression[E]] =
    new CompositeCapturedMatcherExpression[Captured, E, CapturedMatcherExpression[E]](left, this)

  def ->:[E <: Expression](
    left: IsMatcherExpression[E]
  ): CompositeIsMatcherExpression[Captured, E, IsMatcherExpression[E]] =
    new CompositeIsMatcherExpression[Captured, E, IsMatcherExpression[E]](left, this)

  def stackMatches(input: NonEmptyList[Expression]): Either[InconsistencyMeasure, (List[Expression], Captured)]
}

trait PrimitiveMatcherExpression[E <: Expression, Captured <: HList] extends MatcherExpression[Captured]

final class CompositeCapturedMatcherExpression[
  RightCaptured <: HList,
  E <: Expression,
  M <: CapturedMatcherExpression[E]
](
  left: M,
  right: MatcherExpression[RightCaptured])
    extends MatcherExpression[shapeless.::[E, RightCaptured]] {
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Either[InconsistencyMeasure, (List[Expression], shapeless.::[E, RightCaptured])] = {
    val leftResult  = left.stackMatches(input)
    val rightResult = NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Left(Infinite))
    (leftResult, rightResult) match {
      case (Left(i), Right(_)) => Left(i)
      case (Right((_, capturedL)), Right((stack, capturedR))) =>
        Right((stack, shapeless.::(capturedL.head, capturedR)))
      case (Right(_), Left(_))  => Left(Infinite)
      case (Left(il), Left(ir)) => Left(il + ir)
    }
  }
}

final class CompositeIsMatcherExpression[RightCaptured <: HList, E <: Expression, M <: IsMatcherExpression[E]](
  left: M,
  right: MatcherExpression[RightCaptured])
    extends MatcherExpression[RightCaptured] {

  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Either[InconsistencyMeasure, (List[Expression], RightCaptured)] = {
    val leftResult  = left.stackMatches(input)
    val rightResult = NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Left(Infinite))
    (leftResult, rightResult) match {
      case (Left(i), Right(_))         => Left(i)
      case (Right(_), Right(captured)) => Right(captured)
      case (Right(_), Left(_))         => Left(Infinite)
      case (Left(il), Left(ir))        => Left(il + ir)
    }
  }
}

final class IsMatcherExpression[E <: Expression: ClassTag](condition: E => Boolean)
    extends PrimitiveMatcherExpression[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Either[InconsistencyMeasure, (List[Expression], HNil)] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) => Right((tail, HNil))
      case _                                                                        => Left(InconsistencyNumber(1))
    }
  }
}

final class CapturedMatcherExpression[E <: Expression: ClassTag](condition: E => Boolean)
    extends PrimitiveMatcherExpression[E, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Either[InconsistencyMeasure, (List[Expression], shapeless.::[E, HNil])] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) => Right((tail, head :: HNil))
      case _                                                                        => Left(InconsistencyNumber(1))
    }
  }
}

object MatcherExpression {
  def check[T <: Expression: ClassTag]: IsMatcherExpression[T] = checkIf[T](_ => true)

  def checkIf[T <: Expression: ClassTag](condition: T => Boolean): IsMatcherExpression[T] =
    new IsMatcherExpression[T](condition)

  def capture[T <: Expression: ClassTag]: CapturedMatcherExpression[T] = captureIf[T](_ => true)

  def captureIf[T <: Expression: ClassTag](condition: T => Boolean): CapturedMatcherExpression[T] =
    new CapturedMatcherExpression[T](condition)
}
