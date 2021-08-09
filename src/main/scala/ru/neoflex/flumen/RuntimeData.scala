package ru.neoflex.flumen

import shapeless.HList

case class RuntimeData[A] (
  results: Seq[Result[A]],
  services: ContrServiceList[A],
  conditions: Seq[Condition[A]]
  ) {
  def execute (serviceName: String, runtime: HList): HList = {
  ???
  }
}
