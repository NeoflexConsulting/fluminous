package org.fluminous.services

import shapeless.{ ::, HList, HNil }

import scala.annotation.implicitNotFound

@implicitNotFound(
  "Implicit not found: " +
    "IsDistinct[${L}]. " +
    "Service collection already contains appended type"
)
trait IsDistinct[L <: HList] extends Serializable

object IsDistinct {

  def apply[L <: HList](implicit id: IsDistinct[L]): IsDistinct[L] = id

  implicit def hnilIsDistinct: IsDistinct[HNil] = new IsDistinct[HNil] {}

  implicit def hlistIsDistinct[U, L <: HList](implicit d: IsDistinct[L], nc: NotContains[L, U]): IsDistinct[U :: L] =
    new IsDistinct[U :: L] {}
}
