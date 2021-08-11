package org.fluminous.matrix

sealed trait ServicesWithInput2DList {
  type THIS <: ServicesWithInput2DList
  def append[I](services: ServicesWithInput[I]): ServicesWithInput2DCompose[I, THIS]
}

object ServicesWithInput2DNil extends ServicesWithInput2DList {
  type THIS = this.type
  override def append[I](services: ServicesWithInput[I]): ServicesWithInput2DCompose[I, THIS] =
    ServicesWithInput2DCompose(services, ServicesWithInput2DNil)
}

final case class ServicesWithInput2DCompose[I, TAIL <: ServicesWithInput2DList](
  services: ServicesWithInput[I],
  tail: TAIL)
    extends ServicesWithInput2DList {
  type THIS = ServicesWithInput2DCompose[I, TAIL]
  override def append[NEXT_I](services: ServicesWithInput[NEXT_I]): ServicesWithInput2DCompose[NEXT_I, THIS] =
    ServicesWithInput2DCompose(services, this)
}


