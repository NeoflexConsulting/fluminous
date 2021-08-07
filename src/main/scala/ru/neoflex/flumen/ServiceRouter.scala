package ru.neoflex.flumen

import ru.neoflex.flumen.MatrixCompose.InnerMatrixCompose

import scala.reflect.ClassTag

class NotFoundException extends Exception

// TODO Implement Contains and Unique typeclasses

sealed trait ContrServiceList[A] {
  type R <: ContrServiceList[A]
  def last(implicit last : Last[A,R]): Service[A,last.Out]
  def append[H](service: Service[A,H]): ContrCompose[A,H,R]
}

sealed class ContrNil[A] extends ContrServiceList[A] {
  type R = ContrNil[A]
  override def last(implicit last : Last[A,R]): Service[A,last.Out] = last.apply(this)
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,ContrNil[A])
}

object ContrNil {
  def apply[A] = new ContrNil[A]
}

final case class ContrCompose[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : T) extends ContrServiceList[A] {
  type R = ContrCompose[A,H,T]
  override def last(implicit last : Last[A,R]): Service[A,last.Out] = last.apply(this)
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,this)
  def get[U](implicit get: ContrGet[A,R,U]):Service[A,U] = get.apply(this)
}


object ContrServiceList{
  def apply[A,H](service: Service[A,H]) = {ContrCompose[A, H,ContrNil[A]](service,  ContrNil[A])}
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


sealed trait CovServiceList[A] {
  type R <: CovServiceList[A]
  def append[H](service: Service[H,A]): CovCompose[H,A,R]
}

sealed class CovNil[A] extends CovServiceList[A] {
  type R = CovNil[A]
  override def append[H](service: Service[H,A]): CovCompose[H,A,R] = CovCompose(service,CovNil[A])
}

object CovNil {
  def apply[A] = new CovNil[A]
}

final case class CovCompose[H, A, T <: CovServiceList[A]](head : Service[H,A], tail : T) extends CovServiceList[A] {
  type R = CovCompose[H,A,T]
  override def append[H](service: Service[H,A]): CovCompose[H,A,R] = CovCompose(service,this)
  def get[U](implicit get: CovGet[A,R,U]):Service[U,A] = get.apply(this)
}

object CovServiceList {
  def apply[H,A](service: Service[H,A]) ={CovCompose[H,A,CovNil[A]](service,CovNil[A])}
}



sealed trait ServiceMatrix{
  type R <: ServiceMatrix
  type DiagonalType
  type ContrSignature[A] <: ContrServiceList[A]
  type CovSignature[A] <: CovServiceList[A]
}

object NilMatrix extends ServiceMatrix {
  type R = NilMatrix.type
  type DiagonalType = Nothing
  type ContrSignature[A] = ContrNil[A]
  type CovSignature[A] = CovNil[A]
}




object MatrixCompose {
  def apply[H, T <: ServiceMatrix] (tail : T)(service: Service[H,H], servicesColumn : tail.ContrSignature[H], servicesRow: tail.CovSignature[H] ) = {
    InnerMatrixCompose(tail,service,servicesColumn,servicesRow)
  }

  final case class InnerMatrixCompose [H, T <: ServiceMatrix, SCONTR<: ContrServiceList[H], SCOV <: CovServiceList[H]](tail : T,service: Service[H,H], servicesColumn : SCONTR, servicesRow: SCOV ) extends ServiceMatrix {
    type R =  InnerMatrixCompose [H, T, SCONTR, SCOV]
    type DiagonalType = H
    type ContrSignature[A] = ContrCompose[A,DiagonalType,tail.ContrSignature[A]]
    type CovSignature[A] = CovCompose[DiagonalType,A,tail.CovSignature[A]]

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




//Класс типов. Умеет для ковариантного списка L возвращать Service[U,A]
//Нам нужны его инстансы для каждого типа списка L
trait CovGet[A, L <: CovServiceList[A],U] {
  def apply(t: L): Service[U,A]
}

object CovGet {
  def apply[A, L <: CovServiceList[A],U](implicit got: CovGet[A,L,U]): CovGet[A, L, U] = got

  //Инстанс для случая, когда в L тип элемента H совпадает совпадает с искомым U = H, мы просто возвращаем t.head
  implicit def select[A, H, T <: CovServiceList[A]]: CovGet[A, CovCompose[H,A,T],H] =
    new CovGet[A, CovCompose[H,A,T],H] {
      def apply(t : CovCompose[H,A,T]):Service[H,A] = t.head
    }

  //Инстанс для случая, когда L - композиция H и списка T и для T у нас уже есть тайп-класс
  implicit def recurse[A, H, T <: CovServiceList[A], U]
  (implicit st : CovGet[A, T, U]): CovGet[A, CovCompose[H,A, T], U] =
    new CovGet[A, CovCompose[H,A,T], U] {
      def apply(t :CovCompose[H,A,T]):Service[U,A]  = st(t.tail)
    }
}

//Класс типов. Умеет для контрвариантного списка L возвращать Service[A,U]
//Нам нужны его инстансы для каждого типа списка L. Дальнейшая логика аналогична CovGet
trait ContrGet[A, L <: ContrServiceList[A],U] {
  def apply(t: L): Service[A,U]
}

object ContrGet {
  def apply[A, L <: ContrServiceList[A],U](implicit got: ContrGet[A,L,U]): ContrGet[A, L, U] = got

  implicit def select[A, H, T <: ContrServiceList[A]]: ContrGet[A, ContrCompose[A,H,T],H] =
    new ContrGet[A, ContrCompose[A,H,T],H] {
      def apply(t : ContrCompose[A,H,T]):Service[A,H] = t.head
    }

  implicit def recurse[A, H, T <: ContrServiceList[A], U]
  (implicit st : ContrGet[A, T, U]): ContrGet[A, ContrCompose[A,H, T], U] =
    new ContrGet[A, ContrCompose[A,H,T], U] {
      def apply(t :ContrCompose[A,H,T]):Service[A,U]  = st(t.tail)
    }
}

//Класс типов, Умеет для матрицы M возвращать Service[A,U]
trait MatrixGet[M <: ServiceMatrix,I,O] {
  def apply(t: M): Service[I,O]
}
object MatrixGet {
  def apply[M <: ServiceMatrix,I,O](implicit got: MatrixGet[M,I,O]): MatrixGet[M,I,O] = got
  //У нас 3 базовых случая
  //Когда у нас M <: ServiceMatrix,I,O и есть implicit ContrGet[M.ContrSignature, I,O]
  implicit def selectContrvariant[M <: ServiceMatrix,CONTR<: ContrServiceList[I], COV <: CovServiceList[I],I,O](implicit c: ContrGet[I,CONTR,O]):
  MatrixGet[InnerMatrixCompose[I,M,CONTR,COV],I,O] = {
    new MatrixGet[InnerMatrixCompose[I,M,CONTR,COV],I,O] {
      override def apply(t: InnerMatrixCompose[I, M, CONTR, COV]): Service[I, O] = c(t.servicesColumn)
    }
  }
  //Когда у нас M <: ServiceMatrix,I,O и есть implicit CovGet[M.CovSignature, I,O]
  implicit def selectCovariant[M <: ServiceMatrix,CONTR<: ContrServiceList[O], COV <: CovServiceList[O],I,O](implicit c: CovGet[O,COV,I]):
  MatrixGet[InnerMatrixCompose[O,M,CONTR,COV],I,O] = new MatrixGet[InnerMatrixCompose[O,M,CONTR,COV],I,O]{
    def apply(t : InnerMatrixCompose[O,M,CONTR,COV]):Service[I,O] = c(t.servicesRow)
  }

  //Когда у нас M <: ServiceMatrix,M.Diagonal,M.diagonal
  implicit def selectDiagonal[M <: ServiceMatrix,H, CONTR<: ContrServiceList[H], COV <: CovServiceList[H]]:
  MatrixGet[InnerMatrixCompose[H,M,CONTR,COV],H,H] =
    new MatrixGet[InnerMatrixCompose[H,M,CONTR,COV],H,H] {
      def apply(t : InnerMatrixCompose[H,M,CONTR,COV]):Service[H,H] = t.service
    }

  //И видимо должен быть рекурсивный, перебирающий матрицы
}


object Test {
  def main(args: Array[String]): Unit = {
      val serviceMatrix1 = ServiceMatrix(Service[String,String]("upper",_.toUpperCase))
      val service1List = ContrServiceList(Service[Int, List[Int]]("toList",List(_))).
        append(Service[Int, String]("to_string",_.toString))
      println(service1List.get[String].invoke(23))
      println(service1List.get[List[Int]].invoke(23))

    val service2List = CovServiceList(Service[List[Int],Int]("fromList",_.sum)).
        append(Service[String,Int]("to_int",_.toInt))
    println(service2List.get[String].invoke("23"))
    println(service2List.get[List[Int]].invoke(List(23,12)))

      val serviceMatrix2 = serviceMatrix1.enlarge(Service[Int,Int]("increment",_ + 1),
        ContrServiceList(Service[Int, String]("to_string",_.toString)),
        CovServiceList(Service[String,Int]("to_int",_.toInt)))
  }
}
