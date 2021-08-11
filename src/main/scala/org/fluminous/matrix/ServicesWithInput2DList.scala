package org.fluminous.matrix

sealed trait ServicesWithInput2DList {
  type THIS <: ServicesWithInput2DList
  type LIST <: ServicesWithInput[_]
  def append[I, SI <: ServicesWithInput[I]](services: SI): ServicesWithInput2DCompose[I, SI, THIS]
}

object ServicesWithInput2DNil extends ServicesWithInput2DList {
  type THIS = this.type
  type LIST = Nothing
  override def append[I, SI <: ServicesWithInput[I]](services: SI): ServicesWithInput2DCompose[I, SI, THIS] =
    ServicesWithInput2DCompose(services, ServicesWithInput2DNil)
}

final case class ServicesWithInput2DCompose[I, SI <: ServicesWithInput[I], TAIL <: ServicesWithInput2DList](
  services: SI,
  tail: TAIL)
    extends ServicesWithInput2DList {
  type THIS = ServicesWithInput2DCompose[I, SI, TAIL]
  type LIST = services.THIS
  override def append[NEXT_I, NEXT_SI <: ServicesWithInput[NEXT_I]](
    services: NEXT_SI
  ): ServicesWithInput2DCompose[NEXT_I, NEXT_SI, THIS] =
    ServicesWithInput2DCompose(services, this)
}
