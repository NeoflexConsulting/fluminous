package org.fluminous.runtime

import Condition.RuntimeCondition
import org.fluminous.matrix.{ RuntimeService, Service }

import scala.reflect.ClassTag

// TODO Сделать привязку выходного типа к обновлению конкретной ячейки контекста через функцию,
//  что-то типа O => Runtime. Тогда можно вообще отказаться от проверки типов в Runtime

case class TypeInfo(
  typeName: String,
  variables: Map[String, Variable],
  services: Map[String, RuntimeService],
  conditions: Map[String, RuntimeCondition]) {}

object TypeInfo {
  def forType[A: ClassTag]: TypeInfo = {
    TypeInfo(
      implicitly[ClassTag[A]].runtimeClass.getTypeName,
      Map.empty[String, Variable],
      Map.empty[String, RuntimeService],
      Map.empty[String, RuntimeCondition]
    )
  }
  def forType[A: ClassTag](services: Seq[Service[A, _]], conditions: Seq[Condition[A]]): TypeInfo = {
    TypeInfo(
      implicitly[ClassTag[A]].runtimeClass.getTypeName,
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
