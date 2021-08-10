package org.fluminous.matrix

trait GetServiceByInput[SEARCHED,O, LIST <: ServicesWithOutput[O]] {
  def apply(t: LIST): Service[SEARCHED,O]
}

object GetServiceByInput {
  def apply[SEARCHED, O, LIST <: ServicesWithOutput[O]](implicit got: GetServiceByInput[SEARCHED,O,LIST]): GetServiceByInput[SEARCHED, O, LIST] = got

  implicit def select[SEARCHED, O, TAIL <: ServicesWithOutput[O]]: GetServiceByInput[SEARCHED, O,ServicesWithOutputCompose[SEARCHED,O,TAIL]] =
    new GetServiceByInput[SEARCHED, O,ServicesWithOutputCompose[SEARCHED,O,TAIL]] {
      def apply(t : ServicesWithOutputCompose[SEARCHED,O,TAIL]):Service[SEARCHED,O] = t.service
    }

  implicit def recurse[I, O,  TAIL <: ServicesWithOutput[O], SEARCHED]
  (implicit st : GetServiceByInput[SEARCHED, O, TAIL]): GetServiceByInput[SEARCHED, O, ServicesWithOutputCompose[I,O, TAIL]] =
    new GetServiceByInput[SEARCHED, O, ServicesWithOutputCompose[I,O,TAIL]] {
      def apply(t :ServicesWithOutputCompose[I,O,TAIL]):Service[SEARCHED,O]  = st(t.tail)
    }
}

