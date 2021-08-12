package org.fluminous.services

import shapeless.unexpected

trait NotEqual[A, B]

object NotEqual {
  implicit def neq[A, B]: A NotEqual B    = new NotEqual[A, B] {}
  implicit def neqAmbig1[A]: A NotEqual A = unexpected
  implicit def neqAmbig2[A]: A NotEqual A = unexpected
}
