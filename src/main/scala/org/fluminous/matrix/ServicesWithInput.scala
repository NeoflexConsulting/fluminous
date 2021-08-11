package org.fluminous.matrix

import scala.collection.mutable

sealed trait ServicesWithInput[I] {
  type THIS <: ServicesWithInput[I]
  def append[O](services: Seq[Service[I, O]]): ServicesWithInputCompose[I, O, THIS]
  def toSeq: Seq[Service[I, _]] = constructSeq(Seq.empty)
  def constructSeq(currentSeq: Seq[Service[I, _]]): Seq[Service[I, _]]
}

sealed class ServicesWithInputNil[I] extends ServicesWithInput[I] {
  type THIS = ServicesWithInputNil[I]
  override def append[O](services: Seq[Service[I, O]]): ServicesWithInputCompose[I, O, THIS] =
    ServicesWithInputCompose(services.toBuffer, ServicesWithInputNil[I])
  override def constructSeq(currentSeq: Seq[Service[I, _]]): Seq[Service[I, _]] = currentSeq
}

object ServicesWithInputNil {
  def apply[I] = new ServicesWithInputNil[I]
}

final case class ServicesWithInputCompose[I, O, TAIL <: ServicesWithInput[I]](
  head: mutable.Buffer[Service[I, O]],
  tail: TAIL)
    extends ServicesWithInput[I] {
  type THIS = ServicesWithInputCompose[I, O, TAIL]
  override def append[NEXT_O](services: Seq[Service[I, NEXT_O]]): ServicesWithInputCompose[I, NEXT_O, THIS] =
    ServicesWithInputCompose(services.toBuffer, this)
  def getByOutput[SEARCHED](implicit get: GetServiceByOutput[I, SEARCHED, THIS]): Seq[Service[I, SEARCHED]] = get.apply(this)

  override def constructSeq(currentSeq: Seq[Service[I, _]]): Seq[Service[I, _]] = tail.constructSeq(head ++ currentSeq)
}

object ServicesWithInput {
  def apply[A, H](services: Seq[Service[A, H]]) = {
    ServicesWithInputNil[A].append(services)
  }
  def apply[A, H](service: Service[A, H]) = {
    ServicesWithInputNil[A].append(Seq(service))
  }
}
