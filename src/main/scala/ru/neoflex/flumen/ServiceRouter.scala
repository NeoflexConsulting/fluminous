package ru.neoflex.flumen

import scala.reflect.ClassTag

class NotFoundException extends Exception

// TODO Implement Contains and Unique typeclasses

case class Evidence[F](className: String)

object Evidence {
  implicit def evidence[F](implicit classTag: ClassTag[F]):Evidence[F] = new  Evidence[F](classTag.runtimeClass.getName)
  def isEqual[I,O](evidence1: Evidence[I], evidence2: Evidence[O]): Boolean = evidence1.className == evidence2.className
}


sealed trait ContrServiceList[A] {
  def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]]
}

sealed class ContrNil[A] extends ContrServiceList[A] {
  override def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]] = Left(new NotFoundException)
}

object ContrNil {
  def apply[A] = new ContrNil[A]
}

final case class ContrCompose[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : T) extends ContrServiceList[A] {
  override def getService[I:Evidence, O: Evidence](name: String): Either[NotFoundException, Service[I,O]] = {
    if (!head.hasInputType[I]) {
      Left(new NotFoundException)
    } else if (!head.hasOutputType[O] || head.name != name) {
      tail.getService[I,O](name)
    } else {
      Right(head.as[I,O])
    }
  }
}

object ContrServiceList{
  def apply[A,H](service: Service[A,H]) = {ContrCompose[A, H,ContrNil[A]](service,  ContrNil[A])}
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
      val m1 = ServiceMatrix[String](Service("upper",_.toUpperCase))
      val m2 = m1.enlarge[Int](Service("increment",_ + 1),
        ContrServiceList[Int, String](Service("to_string",_.toString)),
        CovServiceList[String,Int](Service("to_int",_.toInt)))
  }
}
