package org.fluminous.jq

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

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
}
