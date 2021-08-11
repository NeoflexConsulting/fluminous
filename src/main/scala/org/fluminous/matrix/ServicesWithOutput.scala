package org.fluminous.matrix

sealed trait ServicesWithOutput[O] {
  type THIS <: ServicesWithOutput[O]
  type L2D <: ServicesWithInput2DList
  def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS]
  def toServices2DList: L2D

}

sealed class ServicesWithOutputNil[O] extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputNil[O]
  type L2D  = ServicesWithInput2DNil.type
  override def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS] =
    ServicesWithOutputCompose(service, ServicesWithOutputNil[O])
  override def toServices2DList: L2D = ServicesWithInput2DNil
}

object ServicesWithOutputNil {
  def apply[O] = new ServicesWithOutputNil[O]
}

final case class ServicesWithOutputCompose[I, O, TAIL <: ServicesWithOutput[O]](service: Service[I, O], tail: TAIL)
    extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputCompose[I, O, TAIL]
  type L2D  = ServicesWithInput2DCompose[I, ServicesWithInputCompose[I, O, ServicesWithInputNil[I]], tail.L2D]

  override def append[NEXT_INPUT](service: Service[NEXT_INPUT, O]): ServicesWithOutputCompose[NEXT_INPUT, O, THIS] =
    ServicesWithOutputCompose(service, this)

  def getByInput[SEARCHED](implicit get: GetServiceByInput[SEARCHED, O, THIS]): Service[SEARCHED, O] = get.apply(this)

  override def toServices2DList: L2D =
    ServicesWithInput2DCompose(ServicesWithInput(service), tail.toServices2DList)
}

object ServicesWithOutput {
  def apply[I, O](service: Service[I, O]) = {
    ServicesWithOutputNil[O].append(service)
  }
}
