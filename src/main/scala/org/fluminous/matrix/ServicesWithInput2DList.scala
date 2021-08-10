package org.fluminous.matrix

sealed trait ServicesWithInput2DList {}

object ServicesWithInput2DNil extends ServicesWithInput2DList

final case class ServicesWithInput2DCompose[I, O, TAIL <: ServicesWithInput2DList](service: Service[I, O], tail: TAIL)
    extends ServicesWithInput2DList
