package ru.neoflex.flumen

import shapeless.HList

case class TypeInfo[A] (
  variables: Seq[Variable[A]],
  services: ServicesWithInput[A],
  conditions: Seq[Condition[A]]
  ) {
  def execute (serviceName: String, runtime: HList): HList = {
  ???
  }
}
