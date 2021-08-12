package org.fluminous.matrix

import shapeless.{::, HList, HNil, NotContainsConstraint}

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Implicit not found: " +
    "shapeless.IsDistinct[${L}]. " +
    "Some elements have the same type."
)
trait IsDistinct[L <: HList] extends Serializable

object IsDistinct {

  def apply[L <: HList](implicit id: IsDistinct[L]): IsDistinct[L] = id

  implicit def hnilIsDistinct = new IsDistinct[HNil] {}

  implicit def hlistIsDistinct[U, L <: HList](
    implicit d: IsDistinct[L],
    nc: NotContains[L, U]
  ): IsDistinct[U :: L] =
    new IsDistinct[U :: L] {}
}
