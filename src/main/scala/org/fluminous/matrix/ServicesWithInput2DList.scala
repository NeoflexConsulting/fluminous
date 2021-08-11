package org.fluminous.matrix

sealed trait ServicesWithInput2DList {
  type THIS <: ServicesWithInput2DList
  type LIST <: ServicesWithInput[_]
  type SO[O] <: ServicesWithOutput[O]
  type APPENDED_THIS[O] <: ServicesWithInput2DList
  def appendColumn[I, SI <: ServicesWithInput[I]](services: SI): ServicesWithInput2DCompose[I, SI, THIS]
  def appendRow[O](servicesWithOutput: SO[O]): APPENDED_THIS[O]
}

object ServicesWithInput2DNil extends ServicesWithInput2DList {
  type THIS             = this.type
  type LIST             = Nothing
  type SO[O]            = ServicesWithOutputNil[O]
  type APPENDED_THIS[O] = ServicesWithInput2DNil.type
  override def appendColumn[I, SI <: ServicesWithInput[I]](services: SI): ServicesWithInput2DCompose[I, SI, THIS] =
    ServicesWithInput2DCompose(services, ServicesWithInput2DNil)
  override def appendRow[O](servicesWithOutput: SO[O]): APPENDED_THIS[O] = ServicesWithInput2DNil
}

final case class ServicesWithInput2DCompose[I, SI <: ServicesWithInput[I], TAIL <: ServicesWithInput2DList](
  services: SI,
  tail: TAIL)
    extends ServicesWithInput2DList {
  type THIS             = ServicesWithInput2DCompose[I, SI, TAIL]
  type LIST             = services.THIS
  type SO[O]            = ServicesWithOutputCompose[I, O, tail.SO[O]]
  type APPENDED_THIS[O] = ServicesWithInput2DCompose[I, ServicesWithInputCompose[I, O, SI], tail.APPENDED_THIS[O]]
  override def appendColumn[NEXT_I, NEXT_SI <: ServicesWithInput[NEXT_I]](
    services: NEXT_SI
  ): ServicesWithInput2DCompose[NEXT_I, NEXT_SI, THIS] =
    ServicesWithInput2DCompose(services, this)

  override def appendRow[O](servicesWithOutput: SO[O]): APPENDED_THIS[O] = {
    ServicesWithInput2DCompose(
      ServicesWithInputCompose(servicesWithOutput.service, services),
      tail.appendRow[O](servicesWithOutput.tail)
    )
  }
  def dropColumn: (SI, TAIL) = {
    (services, tail)
  }
}
