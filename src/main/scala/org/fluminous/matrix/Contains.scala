package org.fluminous.matrix

import scala.annotation.implicitNotFound
import shapeless.{ ::, HList }

@implicitNotFound(
  "Implicit not found: Contains[${L}, ${U}]. " +
    "HList does not contains service type ${U}"
)
trait Contains[L <: HList, U]

object Contains {
  def apply[L <: HList, U](implicit c: Contains[L, U]): Contains[L, U] = c

  implicit def contains[L <: HList, U]: Contains[U :: L, U] =
    new Contains[U :: L, U] {}

  implicit def recurse[L <: HList, T, U](implicit ev: L Contains U): Contains[T :: L, U] =
    new Contains[T :: L, U] {}
}
