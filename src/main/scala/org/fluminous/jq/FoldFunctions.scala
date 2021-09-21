package org.fluminous.jq

import cats.Monoid
import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }

trait FoldFunctions {
  protected def firstValidOrAllInvalids[A, B, C](l: List[A])(f: A => Validated[B, C]): Validated[List[B], C] = {
    val initialState: Validated[List[B], C] = Invalid(List.empty[B])
    l.foldLeft(initialState) {
      case (state, e) =>
        (state, e) match {
          case (v @ Valid(_), _) => v
          case (Invalid(t), el)  => f(el).leftMap(_ +: t)
        }
    }
  }

  protected def foldBoolean[A, B](
    l: List[A]
  )(
    f: A => Either[B, Boolean]
  )(
    monoid: Monoid[Boolean]
  ): Either[B, Boolean] = {
    val initialState: Either[B, Boolean] = Right(monoid.empty)
    l.foldLeft(initialState) {
      case (state, e) =>
        (state, e) match {
          case (e @ Left(_), _) => e
          case (Right(t), el)   => if (t == !monoid.empty) Right(t) else f(el).map(monoid.combine(_, t))
        }
    }
  }

  val And: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a && b
      def empty                           = true
    }

  val Or: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean) = a || b
      def empty                           = false
    }
}
