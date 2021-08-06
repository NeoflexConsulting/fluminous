package ru.neoflex.flumen

abstract class Service[IN:Evidence,OUT:Evidence] {
  val name: String
  def invoke(request: IN): OUT
  def hasInputType[I:Evidence]:Boolean = Evidence.isEqual(implicitly[Evidence[I]],implicitly[Evidence[IN]])
  def hasOutputType[O:Evidence]:Boolean = Evidence.isEqual(implicitly[Evidence[O]],implicitly[Evidence[OUT]])
  def as[I:Evidence,O:Evidence]: Service[I,O] = {
    val outer = this
    new Service[I, O] {
      override val name = outer.name
      override def invoke(request: I): O = {
        outer.invoke(request.asInstanceOf[IN]).asInstanceOf[O]
      }
    }
  }
}

object Service {
  def apply[IN:Evidence, OUT:Evidence](serviceName: String, func: IN => OUT): Service[IN, OUT] = {
    new Service[IN, OUT] {
      override val name: String = serviceName
      override def invoke(request: IN): OUT = func(request)
    }
  }
}
