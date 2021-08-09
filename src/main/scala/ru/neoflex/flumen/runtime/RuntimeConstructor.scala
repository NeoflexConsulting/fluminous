package ru.neoflex.flumen.runtime

import shapeless.HList

sealed case class RuntimeConstructor[R <: HList](runtime: R) {
  type Out = R
}
