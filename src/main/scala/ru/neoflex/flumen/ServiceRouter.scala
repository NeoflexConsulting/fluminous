package ru.neoflex.flumen

sealed trait HContrFunctionList[A]
final case class ::=>[A, H, T <: HContrFunctionList[A]](head : A => H, tail : T) extends HContrFunctionList[A] {
}
sealed class HContrNil[A] extends HContrFunctionList[A] {
}
object HContrNil {
  def apply[A] = new HContrNil[A]
}

object HContrFunctionList{
  def apply[A,H](func: A=> H) = {::=>[A, H,HContrNil[A]](func,  HContrNil[A])}
}


sealed trait HCovFunctionList[A]
final case class ::<=[H, A, T <: HCovFunctionList[A]](head : H => A, tail : T) extends HCovFunctionList[A] {
}
sealed class HCovNil[A] extends HCovFunctionList[A] {
}

object HCovNil {
  def apply[A] = new HCovNil[A]
}

object HCovFunctionList {
  def apply[H,A](func: H=>A) ={::<=[H,A,HCovNil[A]](func,HCovNil[A])}
}



sealed trait HFunctionMatrix{
  type ContrSignature[A] <: HContrFunctionList[A]
  type CovSignature[A] <: HCovFunctionList[A]

}

final case class ::: [H, T <: HFunctionMatrix](tail : T)(diag: H=>H, column : tail.ContrSignature[H], row: tail.CovSignature[H] ) extends HFunctionMatrix {
  type ContrSignature[A] = ::=>[A,H,tail.ContrSignature[A]]
  type CovSignature[A] = ::<=[H,A,tail.CovSignature[A]]
}

object HNilMatrix extends HFunctionMatrix {
  type ContrSignature[A] = HContrNil[A]
  type CovSignature[A] = HCovNil[A]
}

object HFunctionMatrix {
  def apply[W](func: W => W):HFunctionMatrix = {
    :::(HNilMatrix)(func,HContrNil[W], HCovNil[W])
  }
}


object Test {
  def main(args: Array[String]): Unit = {
      val m1 = HFunctionMatrix[String](_.toUpperCase)
      val l1 = HContrFunctionList[Int, String](_.toString)
      val l2 = HCovFunctionList[String,Int](_.toInt)
      val m2 = HFunctionMatrix
  }
}
