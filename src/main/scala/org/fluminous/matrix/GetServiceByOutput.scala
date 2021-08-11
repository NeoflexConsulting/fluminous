package org.fluminous.matrix

trait GetServiceByOutput[I, SEARCHED, LIST <: ServicesWithInput[I]] {
  def apply(list: LIST): Seq[Service[I, SEARCHED]]
}

object GetServiceByOutput {
  def apply[I, SEARCHED, LIST <: ServicesWithInput[I]](
    implicit got: GetServiceByOutput[I, SEARCHED, LIST]
  ): GetServiceByOutput[I, SEARCHED, LIST] = got

  implicit def select[
    I,
    SEARCHED,
    TAIL <: ServicesWithInput[I]
  ]: GetServiceByOutput[I, SEARCHED, ServicesWithInputCompose[I, SEARCHED, TAIL]] =
    new GetServiceByOutput[I, SEARCHED, ServicesWithInputCompose[I, SEARCHED, TAIL]] {
      def apply(t: ServicesWithInputCompose[I, SEARCHED, TAIL]): Seq[Service[I, SEARCHED]] = t.head
    }

  implicit def recurse[I, O, TAIL <: ServicesWithInput[I], SEARCHED](
    implicit st: GetServiceByOutput[I, SEARCHED, TAIL]
  ): GetServiceByOutput[I, SEARCHED, ServicesWithInputCompose[I, O, TAIL]] =
    new GetServiceByOutput[I, SEARCHED, ServicesWithInputCompose[I, O, TAIL]] {
      def apply(t: ServicesWithInputCompose[I, O, TAIL]): Seq[Service[I, SEARCHED]] = st(t.tail)
    }
}
