package org.fluminous.matrix

import scala.collection.mutable

sealed trait ServicesWithOutput[O] {
  type THIS <: ServicesWithOutput[O]
  def append[I](services: Seq[Service[I, O]]): ServicesWithOutputCompose[I, O, THIS]
}

sealed class ServicesWithOutputNil[O] extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputNil[O]
  override def append[I](services: Seq[Service[I, O]]): ServicesWithOutputCompose[I, O, THIS] =
    ServicesWithOutputCompose(services.toBuffer, ServicesWithOutputNil[O])
}

object ServicesWithOutputNil {
  def apply[O] = new ServicesWithOutputNil[O]
}

final case class ServicesWithOutputCompose[I, O, TAIL <: ServicesWithOutput[O]](
  services: mutable.Buffer[Service[I, O]],
  tail: TAIL)
    extends ServicesWithOutput[O] {
  type THIS = ServicesWithOutputCompose[I, O, TAIL]
  override def append[NEXT_INPUT](
    services: Seq[Service[NEXT_INPUT, O]]
  ): ServicesWithOutputCompose[NEXT_INPUT, O, THIS] =
    ServicesWithOutputCompose(services.toBuffer, this)
  def getByInput[SEARCHED](implicit get: GetServiceByInput[SEARCHED, O, THIS]): Seq[Service[SEARCHED, O]] =
    get.apply(this)
}

object ServicesWithOutput {
  def apply[I, O](services: Seq[Service[I, O]]) = {
    ServicesWithOutputNil[O].append(services)
  }
  def apply[I, O](service: Service[I, O]) = {
    ServicesWithOutputNil[O].append(Seq(service))
  }
}
