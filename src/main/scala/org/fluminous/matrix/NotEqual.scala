package org.fluminous.matrix

import shapeless.unexpected

trait NotEqual[A, B]

object NotEqual {
  implicit def notSame[A, B]: A NotEqual B = new NotEqual[A, B] {}
  implicit def same[A]: A NotEqual A       = unexpected
}
