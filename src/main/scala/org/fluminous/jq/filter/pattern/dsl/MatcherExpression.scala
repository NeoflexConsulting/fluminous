package org.fluminous.jq.filter.pattern.dsl

import cats.data.NonEmptyList
import org.fluminous.jq.filter.pattern.ExpressionPattern
import shapeless.{ HList, HNil }

import scala.reflect.ClassTag

sealed trait InconsistencyMeasure {
  def +(r: InconsistencyMeasure): InconsistencyMeasure
}

case class InconsistencyNumber(number: Int) extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = {
    case InconsistencyNumber(ir) => InconsistencyNumber(number + ir)
    case _                       => Infinite
  }
}

case object Infinite extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = this
}

trait Matcher {
  def stackMatches(
    builder: expression.Captured => List[ExpressionPattern]
  )(
    input: NonEmptyList[ExpressionPattern]
  ): Either[InconsistencyMeasure, List[ExpressionPattern]] = {
    expression.stackMatches(input).map(builder) match {
      case Left(i)  => Left(i)
      case Right(r) =>input.toList.drop()
    }
  }
  protected val expression: MatcherExpression
}

trait MatcherExpression {
  type Captured <: HList
  def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured]
}

trait PrimitiveMatcherExpression extends MatcherExpression {
  def where(condition: Captured => Boolean): MatcherExpression = {
    val outer = this
    new MatcherExpression {
      type Captured = outer.Captured
      override def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured] = {
        outer.stackMatches(input).flatMap { captured =>
          if (condition(captured)) {
            Right(captured)
          } else {
            Left(InconsistencyNumber(1))
          }
        }
      }
    }
  }
}

trait IsMatcherExpression extends PrimitiveMatcherExpression {
  override type Captured = HNil
}

trait CapturedMatcherExpression extends PrimitiveMatcherExpression {
  type CapturedPrimitive
  override type Captured = shapeless.::[CapturedPrimitive, HNil]
}

object MatcherExpression {
  def is[T <: ExpressionPattern: ClassTag]: IsMatcherExpression = new IsMatcherExpression {
    private val clazz = implicitly[ClassTag[T]].runtimeClass
    override def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured] = {
      input match {
        case NonEmptyList(head: T, _) if clazz.isInstance(head) => Right(HNil)
        case _                                                  => Left(InconsistencyNumber(1))
      }
    }
  }

  def capture[T <: ExpressionPattern: ClassTag]: CapturedMatcherExpression = new CapturedMatcherExpression {
    private val clazz = implicitly[ClassTag[T]].runtimeClass
    type CapturedPrimitive = T
    override def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured] = {
      input match {
        case NonEmptyList(head: T, _) if clazz.isInstance(head) => Right(head :: HNil)
        case _                                                  => Left(InconsistencyNumber(1))
      }
    }
  }

  def ::(left: IsMatcherExpression, right: MatcherExpression): MatcherExpression = new MatcherExpression {
    type Captured = right.Captured
    override def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured] = {
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

  def ::(left: CapturedMatcherExpression, right: MatcherExpression): MatcherExpression = new MatcherExpression {
    type Captured = shapeless.::[left.CapturedPrimitive, right.Captured]
    override def stackMatches(input: NonEmptyList[ExpressionPattern]): Either[InconsistencyMeasure, Captured] = {
      val leftResult  = left.stackMatches(input)
      val rightResult = NonEmptyList.fromList(input.tail).map(right.stackMatches(_)).getOrElse(Left(Infinite))
      (leftResult, rightResult) match {
        case (Left(i), Right(_))                  => Left(i)
        case (Right(capturedL), Right(capturedR)) => Right(shapeless.::(capturedL.head, capturedR))
        case (Right(_), Left(_))                  => Left(Infinite)
        case (Left(il), Left(ir))                 => Left(il + ir)
      }
    }
  }

}
