package ru.neoflex.flumen

import ru.neoflex.flumen
import ru.neoflex.flumen.ContrServiceList.Aux

import scala.reflect.ClassTag

class NotFoundException extends Exception

// TODO Implement Contains and Unique typeclasses

case class Evidence[F](className: String)

object Evidence {
  implicit def evidence[F](implicit classTag: ClassTag[F]):Evidence[F] = new  Evidence[F](classTag.runtimeClass.getName)
  def isEqual[I,O](evidence1: Evidence[I], evidence2: Evidence[O]): Boolean = evidence1.className == evidence2.className
}



sealed trait ContrServiceList[A] {
  type R <: ContrServiceList[A]
  def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]]
  def last(implicit last : Last[A,R]): Service[A,last.Out]
  def append[H](service: Service[A,H]): ContrCompose[A,H,R]
}

sealed class ContrNil[A] extends ContrServiceList[A] {
  type R = ContrNil[A]
  override def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]] = Left(new NotFoundException)
  override def last(implicit last : Last[A,R]): Service[A,last.Out] = last.apply(this)
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,ContrNil[A])
}

object ContrNil {
  def apply[A] = new ContrNil[A]
}

final case class ContrCompose[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : T) extends ContrServiceList[A] {
  type R = ContrCompose[A,H,T]
  override def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]] = {
    if (!head.hasInputType[I]) {
      Left(new NotFoundException)
    } else if (!head.hasOutputType[O] || head.name != name) {
      tail.getService[I,O](name)
    } else {
      Right(head.as[I,O])
    }
  }
  override def last(implicit last : Last[A,R]): Service[A,last.Out] = last.apply(this)
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,this)
}

object ContrCompose {
  def apply2[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : ContrServiceList.Aux[A, T]): ContrCompose[A, H, Aux[A, T]] = {
    ContrCompose(head,tail)
  }
}


object ContrServiceList{
  def apply[A,H](service: Service[A,H]) = {ContrCompose[A, H,ContrNil[A]](service,  ContrNil[A])}
  type Aux[A, Out0] = ContrServiceList[A] { type Out = Out0 }
}

trait Last[A, L <: ContrServiceList[A]] {
  type Out
  def apply(t: L): Service[A,Out]
}

object Last {
  def apply[A, L <: ContrServiceList[A]](implicit last: Last[A,L]): Aux[A, L, last.Out] = last
  type Aux[A, L <: ContrServiceList[A], Out0] = Last[A,L] { type Out = Out0 }

  implicit def hsingleLast[A,H]: Aux[A, ContrCompose[A, H,ContrNil[A]], H] =
    new Last[A, ContrCompose[A, H,ContrNil[A]]] {
      type Out = H
      def apply(l : ContrCompose[A,H,ContrNil[A]]): Service[A,Out] = l.head
    }

  implicit def hlistLast[A, H, T <: ContrServiceList[A], OutT]
  (implicit lt : Last.Aux[A, T, OutT]): Aux[A, ContrCompose[A,H,T], OutT] =
    new Last[A, ContrCompose[A,H,T]] {
      type Out = OutT
      def apply(l : ContrCompose[A,H,T]): Service[A,Out] = lt(l.tail)
    }

}


sealed trait CovServiceList[A]

sealed class CovNil[A] extends CovServiceList[A] {
}

object CovNil {
  def apply[A] = new CovNil[A]
}

final case class CovCompose[H, A, T <: CovServiceList[A]](head : Service[H,A], tail : T) extends CovServiceList[A] {
}

object CovServiceList {
  def apply[H,A](service: Service[H,A]) ={CovCompose[H,A,CovNil[A]](service,CovNil[A])}
}



sealed trait ServiceMatrix{
  type ContrSignature[A] <: ContrServiceList[A]
  type CovSignature[A] <: CovServiceList[A]
}

object NilMatrix extends ServiceMatrix {
  type ContrSignature[A] = ContrNil[A]
  type CovSignature[A] = CovNil[A]
}




object MatrixCompose {
  def apply[H, T <: ServiceMatrix] (tail : T)(service: Service[H,H], servicesColumn : tail.ContrSignature[H], servicesRow: tail.CovSignature[H] ) = {
    InnerMatrixCompose(tail,service,servicesColumn,servicesRow)
  }

  protected final case class InnerMatrixCompose [H, T <: ServiceMatrix, SCONTR<: ContrServiceList[H], SCOV <: CovServiceList[H]](tail : T,service: Service[H,H], servicesColumn : SCONTR, servicesRow: SCOV ) extends ServiceMatrix {
    type ContrSignature[A] = ContrCompose[A,H,tail.ContrSignature[A]]
    type CovSignature[A] = CovCompose[H,A,tail.CovSignature[A]]

    def enlarge[A](service: Service[A,A], servicesColumn : this.ContrSignature[A], servicesRow: this.CovSignature[A]) = {
      MatrixCompose(this)(service,servicesColumn,servicesRow)
    }
  }
}

object ServiceMatrix {
  def apply[W](service: Service[W,W]) = {
    MatrixCompose(NilMatrix)(service,ContrNil[W], CovNil[W])
  }
}


object Test {
  def main(args: Array[String]): Unit = {
      val m1 = ServiceMatrix(Service[String,String]("upper",_.toUpperCase))
      val sd = ContrServiceList(Service[Int, List[Int]]("toList",List(_)))
      val nextSd = sd.append(Service[Int, String]("to_string",_.toString))
      val service1 = sd.last
      println(service1.invoke(23))
      val service2 = nextSd.last
      println(service2.invoke(23))
      val m2 = m1.enlarge[Int](Service("increment",_ + 1),
        ContrServiceList[Int, String](Service("to_string",_.toString)),
        CovServiceList[String,Int](Service("to_int",_.toInt)))
  }
}
