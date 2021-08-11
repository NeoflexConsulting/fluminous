package org.fluminous.matrix

sealed trait ServicesWithOutput[O] {
  type THIS <: ServicesWithOutput[O]
  type L2D <: ServicesWithInput2DList
  type APPENDED_L2D <: ServicesWithInput2DList
  type L2D_SCHEME[A <: CONSTRAINT_TYPE] <: ServicesWithInput2DList
  type CONSTRAINT_TYPE
  def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS]
  def appendTo2DList[L <: CONSTRAINT_TYPE](d2List: L2D_SCHEME[L]): APPENDED_L2D
  def toServices2DList: L2D

}

sealed class ServicesWithOutputNil[O] extends ServicesWithOutput[O] {
  type THIS                             = ServicesWithOutputNil[O]
  type L2D                              = ServicesWithInput2DNil.type
  type APPENDED_L2D                     = ServicesWithInput2DNil.type
  type L2D_SCHEME[A <: CONSTRAINT_TYPE] = ServicesWithInput2DNil.type
  type CONSTRAINT_TYPE                  = ServicesWithInput[_]
  override def append[I](service: Service[I, O]): ServicesWithOutputCompose[I, O, THIS] =
    ServicesWithOutputCompose(service, ServicesWithOutputNil[O])
  override def appendTo2DList[L <: CONSTRAINT_TYPE](d2List: L2D_SCHEME[L]): APPENDED_L2D = ServicesWithInput2DNil
  override def toServices2DList: L2D                                                     = ServicesWithInput2DNil
}

object ServicesWithOutputNil {
  def apply[O] = new ServicesWithOutputNil[O]
}

final case class ServicesWithOutputCompose[I, O, TAIL <: ServicesWithOutput[O]](service: Service[I, O], tail: TAIL)
    extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputCompose[I, O, TAIL]
  type L2D  = L2D_SCHEME[ServicesWithInputCompose[I, O, ServicesWithInputNil[I]]]
  type APPENDED_L2D =
    ServicesWithInput2DCompose[I, ServicesWithInputCompose[I, O, L2D#LIST], tail.APPENDED_L2D]
  type L2D_SCHEME[A <: CONSTRAINT_TYPE] = ServicesWithInput2DCompose[I, A, tail.L2D]
  type CONSTRAINT_TYPE                  = ServicesWithInput[I]

  override def append[NEXT_INPUT](service: Service[NEXT_INPUT, O]): ServicesWithOutputCompose[NEXT_INPUT, O, THIS] =
    ServicesWithOutputCompose(service, this)
  def getByInput[SEARCHED](implicit get: GetServiceByInput[SEARCHED, O, THIS]): Service[SEARCHED, O] = get.apply(this)
  override def appendTo2DList[L <: ServicesWithInput[I]](d2List: L2D_SCHEME[L]): APPENDED_L2D = {
    /*val ds = ServicesWithInputCompose(service, d2List.services)
    ServicesWithInput2DCompose(ds, tail.appendTo2DList(d2List.tail))*/
    ???
  }

  override def toServices2DList: L2D =
    ServicesWithInput2DCompose(ServicesWithInput(service), tail.toServices2DList)
}

object ServicesWithOutput {
  def apply[I, O](service: Service[I, O]) = {
    ServicesWithOutputNil[O].append(service)
  }
}
