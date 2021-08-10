package org.fluminous.matrix

sealed trait ServicesWithOutput[O] {
  type THIS <: ServicesWithOutput[O]
  def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS]
}

sealed class ServicesWithOutputNil[O] extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputNil[O]
  override def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS] =
    ServicesWithOutputCompose(service, ServicesWithOutputNil[O])
}

object ServicesWithOutputNil {
  def apply[O] = new ServicesWithOutputNil[O]
}

final case class ServicesWithOutputCompose[I, O, TAIL <: ServicesWithOutput[O]](service: Service[I, O], tail: TAIL)
    extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputCompose[I, O, TAIL]
  override def append[NEXT_INPUT](service: Service[NEXT_INPUT, O]): ServicesWithOutputCompose[NEXT_INPUT, O, THIS] =
    ServicesWithOutputCompose(service, this)
  def getByInput[SEARCHED](implicit get: GetServiceByInput[SEARCHED, O, THIS]): Service[SEARCHED, O] = get.apply(this)
}

object ServicesWithOutput {
  def apply[I, O](service: Service[I, O]) = {
    ServicesWithOutputCompose[I, O, ServicesWithOutputNil[O]](service, ServicesWithOutputNil[O])
  }
}
