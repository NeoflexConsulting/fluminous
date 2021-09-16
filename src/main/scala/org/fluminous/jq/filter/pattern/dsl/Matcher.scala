package org.fluminous.jq.filter.pattern.dsl

import cats.data.NonEmptyList
import org.fluminous.jq.{ Description, Expression }
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait Matcher[Captured <: HList] extends MatcherReport {
  def ifValidReplaceBy(
    builder: Captured => List[Expression]
  ): NonEmptyList[Expression] => Either[InconsistencyMeasure, List[Expression]] = { input =>
    stackMatches(input).map { case (stack, captured) => builder(captured) ++ stack }
  }
  def ->:[E <: Expression](left: CapturedMatcher[E]): CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]] =
    new CompositeCaptureMatcher[Captured, E, CapturedMatcher[E]](left, this)

  def ->:[E <: Expression](left: IsMatcher[E]): CompositeIsMatcher[Captured, E, IsMatcher[E]] =
    new CompositeIsMatcher[Captured, E, IsMatcher[E]](left, this)

  def stackMatches(input: NonEmptyList[Expression]): Either[InconsistencyMeasure, (List[Expression], Captured)]
}

abstract class BasicMatcher[E <: Expression: Description, Captured <: HList] extends Matcher[Captured] {
  val description: String                          = implicitly[Description[E]].description
  def matchError(actual: List[Expression]): String = { ??? }
}

abstract class CompositeMatcher[M <: BasicMatcher[_, _], RightCaptured <: HList, Captured <: HList](
  left: M,
  right: Matcher[RightCaptured])
    extends Matcher[Captured] {
  def matchError(actual: List[Expression]): String = { ??? }
}

final class CompositeCaptureMatcher[RightCaptured <: HList, E <: Expression, M <: CapturedMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[M, RightCaptured, shapeless.::[E, RightCaptured]](left, right) {
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

final class CompositeIsMatcher[RightCaptured <: HList, E <: Expression, M <: IsMatcher[E]](
  left: M,
  right: Matcher[RightCaptured])
    extends CompositeMatcher[M, RightCaptured, RightCaptured](left, right) {

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

final class IsMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean) extends BasicMatcher[E, HNil] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(input: NonEmptyList[Expression]): Either[InconsistencyMeasure, (List[Expression], HNil)] = {
    input match {
      case NonEmptyList(head: E, tail) if clazz.isInstance(head) && condition(head) => Right((tail, HNil))
      case _                                                                        => Left(InconsistencyNumber(1))
    }
  }
}

final class CapturedMatcher[E <: Expression: ClassTag: Description](condition: E => Boolean)
    extends BasicMatcher[E, shapeless.::[E, HNil]] {
  private val clazz = implicitly[ClassTag[E]].runtimeClass
  override def stackMatches(
    input: NonEmptyList[Expression]
  ): Either[InconsistencyMeasure, (List[Expression], shapeless.::[E, HNil])] = {
    input match {
      case NonEmptyList(head, tail) if clazz.isInstance(head) && condition(head.asInstanceOf[E]) =>
        Right((tail, head.asInstanceOf[E] :: HNil))
      case _ => Left(InconsistencyNumber(1))
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
