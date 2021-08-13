package org.fluminous.runtime

import org.fluminous.services.{ Condition, RuntimeService, Service }
import cats.Monad
import org.fluminous.services.Condition.RuntimeCondition

import scala.reflect.ClassTag

case class TypeInfo[F[_]: Monad](
  typeName: String,
  inputValue: Option[Any],
  variables: Map[String, Variable],
  services: Map[String, RuntimeService[F]],
  conditions: Map[String, RuntimeCondition]) {}

object TypeInfo {
  def forType[F[_]: Monad, A: ClassTag]: TypeInfo[F] = {
    TypeInfo(
      implicitly[ClassTag[A]].runtimeClass.getTypeName,
      None,
      Map.empty[String, Variable],
      Map.empty[String, RuntimeService[F]],
      Map.empty[String, RuntimeCondition]
    )
  }
  def forType[F[_]: Monad, A: ClassTag](services: Seq[Service[F, A, _]], conditions: Seq[Condition[A]]): TypeInfo[F] = {
    TypeInfo(
      implicitly[ClassTag[A]].runtimeClass.getTypeName,
      None,
      Map.empty[String, Variable],
      services.groupBy(_.name).flatMap {
        case (name, services) => services.headOption.map(s => (name, s.toRuntimeService))
      },
      conditions.groupBy(_.conditionName).flatMap {
        case (name, conditions) => conditions.headOption.map(c => (name, c.toRuntimeCondition))
      }
    )
  }
}
