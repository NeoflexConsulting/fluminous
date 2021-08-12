package org.fluminous.matrix

import shapeless.{ ::, HList, HNil }

trait NotContains[L <: HList, U]

object NotContains {
  def apply[L <: HList, U](implicit nc: NotContains[L, U]): NotContains[L, U] = nc

  implicit def nilNotContains[F[_], U]: NotContains[HNil, U] =
    new NotContains[HNil, U] {}

  implicit def recurse[L <: HList, T, U](implicit ev: NotContains[L, U], ev2: U NotEqual T): NotContains[T :: L, U] =
    new NotContains[T :: L, U] {}
}
