package ru.neoflex.flumen

import shapeless.HList

sealed case class RuntimeConstructor[R <: HList](runtime: R) {
  type Out = R
}
