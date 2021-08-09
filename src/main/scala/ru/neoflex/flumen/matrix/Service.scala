package ru.neoflex.flumen.matrix

abstract class Service[IN,OUT] {
  val name: String
  def invoke(request: IN): OUT
}

object Service {
  def apply[IN, OUT](serviceName: String, func: IN => OUT): Service[IN, OUT] = {
    new Service[IN, OUT] {
      override val name: String = serviceName
      override def invoke(request: IN): OUT = func(request)
    }
  }
}
