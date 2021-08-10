package org.fluminous.runtime

import org.fluminous.runtime.exception.{ IncompatibleTypeException, ServiceException }

import scala.reflect.ClassTag

case class Condition[A: ClassTag](
  conditionName: String,
  condition: A => Either[ServiceException, Boolean],
  inputTypeName: String) {

  def this(conditionName: String, condition: A => Either[ServiceException, Boolean]) = {
    this(conditionName, condition, implicitly[ClassTag[A]].runtimeClass.getClass.getTypeName)
  }

  def toRuntimeCondition: Condition[Variable] = {
    val outer = this
    def runtimeCondition(variable: Variable): Either[ServiceException, Boolean] = {
      if (variable.typeName != this.inputTypeName)
        Left(IncompatibleTypeException(this.conditionName, this.inputTypeName, variable.typeName))
      else
        outer.condition(variable.value.asInstanceOf[A])
    }
    new Condition[Variable](conditionName, runtimeCondition, this.inputTypeName)
  }
}

object Condition {
  type RuntimeCondition = Condition[Variable]
}
