package ru.neoflex.flumen.matrix

sealed trait ContrService2DList {
  type R <: ContrService2DList
  def append[I,O](service: Service[I,O]): Contr2DCompose[I,O,R]
}

object Contr2DNil extends ContrService2DList {
  type R = Contr2DNil.type
  override def append[I,O](service: Service[I,O]): Contr2DCompose[I,O,R] = Contr2DCompose(service,Contr2DNil)
}

final case class Contr2DCompose[I,O, T <: ContrService2DList](head : Service[I,O], tail : T) extends ContrService2DList {
  type R = Contr2DCompose[I,O,T]
  override def append[I,O](service: Service[I,O]): Contr2DCompose[I,O,R] = Contr2DCompose(service,this)
}