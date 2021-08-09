package ru.neoflex.flumen

import ru.neoflex.flumen.MatrixCompose.InnerMatrixCompose
import shapeless.{HList, HNil}
class NotFoundException extends Exception

// TODO Implement Unique typeclasses

sealed trait ContrServiceList[A] {
  type R <: ContrServiceList[A]
  def append[H](service: Service[A,H]): ContrCompose[A,H,R]
}

sealed class ContrNil[A] extends ContrServiceList[A] {
  type R = ContrNil[A]
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,ContrNil[A])
}

object ContrNil {
  def apply[A] = new ContrNil[A]
}

final case class ContrCompose[A, H, T <: ContrServiceList[A]](head : Service[A,H], tail : T) extends ContrServiceList[A] {
  type R = ContrCompose[A,H,T]
  override def append[H](service: Service[A,H]): ContrCompose[A,H,R] = ContrCompose(service,this)
  def get[U](implicit get: ContrGet[A,R,U]):Service[A,U] = get.apply(this)
}


object ContrServiceList{
  def apply[A,H](service: Service[A,H]) = {ContrCompose[A, H,ContrNil[A]](service,  ContrNil[A])}
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
  def apply[H, T <: ServiceMatrix] (tail : T)(service: Service[H,H],  conditions: Seq[Condition[H]], servicesColumn : tail.ContrSignature[H], servicesRow: tail.CovSignature[H] ) = {
    InnerMatrixCompose(tail,service,conditions, servicesColumn,servicesRow)
  }

  final case class InnerMatrixCompose [H, T <: ServiceMatrix, SCONTR<: ContrServiceList[H],
      SCOV <: CovServiceList[H]](tail : T,service: Service[H,H], conditions: Seq[Condition[H]], servicesColumn : SCONTR, servicesRow: SCOV ) extends ServiceMatrix {
    type R =  InnerMatrixCompose [H, T, SCONTR, SCOV]
    type DiagonalType = H
    type ContrSignature[A] = ContrCompose[A,DiagonalType,tail.ContrSignature[A]]
    type CovSignature[A] = CovCompose[DiagonalType,A,tail.CovSignature[A]]

    def enlarge[A](servicesColumn : this.ContrSignature[A], servicesRow: this.CovSignature[A], service: Service[A,A], conditions: Seq[Condition[A]] = Seq.empty[Condition[A]]) = {
      MatrixCompose(this)(service,conditions,servicesColumn,servicesRow)
    }
    def get[I,O](implicit get: MatrixGet[R,I,O]):Service[I,O] = get(this)

    // rows = гетерогенный список совариантных сервисов, у которых первый параметры имеет вид Service[H,A]
 /*   protected def appendRuntimeData[R<:HList, T<: HList](runtime: R, rows: Seq[CovServiceList[]] ) = {
           ???
    }*/

    def toRouterRuntime(): HList = {
      val services = this.servicesColumn.append(this.service)
      val runtimeData = RuntimeData[H](Seq.empty[Result[H]], services, this.conditions)
      val runtime = runtimeData :: HNil
      val rows = this.servicesRow :: HNil
      val runtimeConstructor = new RuntimeConstructor(runtime)
      ???
    }
  }
}

object ServiceMatrix {
  def apply[W](service: Service[W,W], condition: Condition[W]) = {
    MatrixCompose(NilMatrix)(service,Seq(condition), ContrNil[W], CovNil[W])
  }
  def apply[W](service: Service[W,W]) = {
    MatrixCompose(NilMatrix)(service,Seq.empty[Condition[W]], ContrNil[W], CovNil[W])
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

  //И рекурсивный случай, перебирающий матрицы
  implicit def recurse[M <: ServiceMatrix,H, CONTR<: ContrServiceList[H], COV <: CovServiceList[H], I,O]
  (implicit mt : MatrixGet[M, I, O]): MatrixGet[InnerMatrixCompose[H,M,CONTR,COV],I,O] =
    new MatrixGet[InnerMatrixCompose[H,M,CONTR,COV],I,O] {
      def apply(t :InnerMatrixCompose[H,M,CONTR,COV]):Service[I,O]  = mt(t.tail)
    }
}


object Test {
  def main(args: Array[String]): Unit = {
    val upperCaseService = Service[String,String]("upper",_.toUpperCase)
    val incrementService = Service[Int,Int]("increment",_ + 1)
    val toStringService = Service[Int, String]("to_string",_.toString)
    val toIntService = Service[String,Int]("to_int",_.toInt)

    val serviceMatrix = ServiceMatrix(upperCaseService)
    val bigServiceMatrix = serviceMatrix.
          enlarge(ContrServiceList(toStringService), CovServiceList(toIntService),incrementService)

    println(serviceMatrix.get[String,String].invoke("some String from Service Matrix1"))
    println(bigServiceMatrix.get[Int,Int].invoke(3))
    println(bigServiceMatrix.get[Int,String].invoke(3))
    println(bigServiceMatrix.get[String,Int].invoke("3"))
    println(bigServiceMatrix.get[String,String].invoke("some String"))
  }
}
