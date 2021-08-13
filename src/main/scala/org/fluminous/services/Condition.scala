package org.fluminous.services

import org.fluminous.runtime.Variable
import org.fluminous.runtime.exception.{IncompatibleTypeException, ServiceException}

import scala.reflect.ClassTag

case class Condition[A: ClassTag](
  conditionName: String,
  check: A => Either[ServiceException, Boolean],
  inputTypeName: String) {

  def this(conditionName: String, condition: A => Either[ServiceException, Boolean]) = {
    this(conditionName, condition, implicitly[ClassTag[A]].runtimeClass.getClass.getTypeName)
  }

  def toRuntimeCondition: Condition[Variable] = {
    val outer = this
    def runtimeCondition(variable: Variable): Either[ServiceException, Boolean] = {
      if (variable.typeName != this.inputTypeName)
        Left(new IncompatibleTypeException(this.conditionName, this.inputTypeName, variable.typeName))
      else
        outer.check(variable.value.asInstanceOf[A])
    }
    new Condition[Variable](conditionName, runtimeCondition, this.inputTypeName)
  }
}

object Condition {
  type RuntimeCondition = Condition[Variable]
}
