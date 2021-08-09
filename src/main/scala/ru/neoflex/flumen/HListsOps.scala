package ru.neoflex.flumen

import shapeless.{HList, HNil, Poly1}

object HListsOps {
  object toContrvariant extends Poly1 {
      implicit def allCases[I,O]: Case.Aux[Service[I,O], ContrServiceList[I]] = at(ContrServiceList(_))
    }
  object pack extends Poly1 {
    implicit def allCases[A]: Case.Aux[A,List[A]] = at(List(_))
  }

  def main(args: Array[String]): Unit = {
    (1 ::232 :: "strtr" :: HNil).map(pack)
  }
}
