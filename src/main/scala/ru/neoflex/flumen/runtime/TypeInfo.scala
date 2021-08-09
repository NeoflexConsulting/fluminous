package ru.neoflex.flumen.runtime

import ru.neoflex.flumen.matrix.ServicesWithInput
import shapeless.HList

case class TypeInfo[A](variables: Seq[Variable[A]], services: ServicesWithInput[A], conditions: Seq[Condition[A]]) {
  def execute(serviceName: String, runtime: HList): HList = {
    ???
  }
}
