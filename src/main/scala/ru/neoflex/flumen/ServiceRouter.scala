package ru.neoflex.flumen

sealed trait ContrServiceList[A]
final case class ::=>[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : T) extends ContrServiceList[A] {
}
sealed class ContrNil[A] extends ContrServiceList[A] {
}
object ContrNil {
  def apply[A] = new ContrNil[A]
}

object ContrServiceList{
  def apply[A,H](service: Service[A,H]) = {::=>[A, H,ContrNil[A]](service,  ContrNil[A])}
}


sealed trait CovServiceList[A]
final case class ::<=[H, A, T <: CovServiceList[A]](head : Service[H,A], tail : T) extends CovServiceList[A] {
}
sealed class CovNil[A] extends CovServiceList[A] {
}

object CovNil {
  def apply[A] = new CovNil[A]
}

object CovServiceList {
  def apply[H,A](service: Service[H,A]) ={::<=[H,A,CovNil[A]](service,CovNil[A])}
}



sealed trait ServiceMatrix{
  type ContrSignature[A] <: ContrServiceList[A]
  type CovSignature[A] <: CovServiceList[A]
}

final case class ::: [H, T <: ServiceMatrix](tail : T)(service: Service[H,H], servicesColumn : tail.ContrSignature[H], servicesRow: tail.CovSignature[H] ) extends ServiceMatrix {
  type ContrSignature[A] = ::=>[A,H,tail.ContrSignature[A]]
  type CovSignature[A] = ::<=[H,A,tail.CovSignature[A]]
  def enlarge[A](service: Service[A,A], servicesColumn : this.ContrSignature[A], servicesRow: this.CovSignature[A]) = {
    :::(this)(service,servicesColumn,servicesRow)
  }
}

object NilMatrix extends ServiceMatrix {
  type ContrSignature[A] = ContrNil[A]
  type CovSignature[A] = CovNil[A]
}

object ServiceMatrix {
  def apply[W](service: Service[W,W]) = {
    :::(NilMatrix)(service,ContrNil[W], CovNil[W])
  }
}


object Test {
  def main(args: Array[String]): Unit = {
      val m1 = ServiceMatrix[String](Service("upper",_.toUpperCase))
      val m2 = m1.enlarge[Int](Service("increment",_ + 1),
        ContrServiceList[Int, String](Service("to_string",_.toString)),
        CovServiceList[String,Int](Service("to_int",_.toInt)))
  }
}
