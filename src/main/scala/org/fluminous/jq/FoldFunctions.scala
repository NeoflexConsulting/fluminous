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

  protected def firstValidOrAllInvalidsWithEither[Input1, Error, Result, Input2, FatalError](
    l: List[Input1],
    init: Input2
  )(
    f: (Input1, Input2) => Either[FatalError, (Input2, Validated[Error, Result])]
  ): Either[FatalError, (Input2, Validated[List[Error], Result])] = {
    val initialState: Either[FatalError, (Input2, Validated[List[Error], Result])] =
      Right(init, Invalid(List.empty[Error]))
    l.foldLeft(initialState) {
      case (state, e) =>
        (state, e) match {
          case (Left(ex), _) =>
            Left(ex)
          case (Right((d, v @ Valid(_))), _) =>
            Right((d, v))
          case (Right((d, Invalid(t))), el) =>
            f(el, d).map { case (i2, r) => (i2, r.leftMap(_ +: t)) }
        }
    }
  }

  protected def firstValidOrAllInvalids[A, B, C, D](
    l: List[A],
    init: D
  )(
    f: (A, D) => (D, Validated[B, C])
  ): (D, Validated[List[B], C]) = {
    val initialState: Validated[List[B], C] = Invalid(List.empty[B])
    l.foldLeft((init, initialState)) {
      case (state, e) =>
        (state, e) match {
          case ((d, v @ Valid(_)), _) => (d, v)
          case ((d, Invalid(t)), el) =>
            val res = f(el, d)
            (res._1, res._2.leftMap(_ +: t))
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
