package org.fluminous.runtime

import org.fluminous.runtime.exception.{
  ConditionNotFoundException,
  ExecutionRuntimeException,
  InputValueNotFoundException,
  ServiceExecutionException,
  ServiceNotFoundException,
  VariableNotFoundException
}

class ExecutionRuntime[Rs](
  private val runtime: Map[String, TypeInfo],
  private val outputGetter: (String, Map[String, TypeInfo]) => Either[ExecutionRuntimeException, Rs]) {
  def executeFirstService(
    serviceName: String,
    outputVariableName: String
  ): Either[ExecutionRuntimeException, ExecutionRuntime[Rs]] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.services.contains(serviceName))
                   .toRight(new ServiceNotFoundException(serviceName))
      inputValue    <- typeInfo._2.inputValue.toRight(new InputValueNotFoundException(typeInfo._2.typeName))
      inputVariable = Variable("input", typeInfo._1, inputValue)
      service       <- typeInfo._2.services.get(serviceName).toRight(new ServiceNotFoundException(serviceName))
      result        <- service.invoke(inputVariable, outputVariableName).left.map(new ServiceExecutionException(_))
    } yield { new ExecutionRuntime[Rs](updateVariable(runtime, result), outputGetter) }
  }

  def executeFirstCondition(conditionName: String): Either[ExecutionRuntimeException, Boolean] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.conditions.contains(conditionName))
                   .toRight(new ConditionNotFoundException(conditionName))
      inputValue    <- typeInfo._2.inputValue.toRight(new InputValueNotFoundException(typeInfo._2.typeName))
      inputVariable = Variable("input", typeInfo._1, inputValue)
      condition     <- typeInfo._2.conditions.get(conditionName).toRight(new ConditionNotFoundException(conditionName))
      result        <- condition.check(inputVariable).left.map(new ServiceExecutionException(_))
    } yield {
      result
    }
  }

  def executeService(
    serviceName: String,
    inputVariableName: String,
    outputVariableName: String
  ): Either[ExecutionRuntimeException, ExecutionRuntime[Rs]] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.services.contains(serviceName))
                   .toRight(new ServiceNotFoundException(serviceName))
      inputVariable <- typeInfo._2.variables
                        .get(inputVariableName)
                        .toRight(new VariableNotFoundException(inputVariableName, typeInfo._1))
      service <- typeInfo._2.services.get(serviceName).toRight(new ServiceNotFoundException(serviceName))
      result  <- service.invoke(inputVariable, outputVariableName).left.map(new ServiceExecutionException(_))
    } yield { new ExecutionRuntime[Rs](updateVariable(runtime, result), outputGetter) }
  }

  def executeCondition(conditionName: String, inputVariableName: String): Either[ExecutionRuntimeException, Boolean] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.conditions.contains(conditionName))
                   .toRight(new ConditionNotFoundException(conditionName))
      inputVariable <- typeInfo._2.variables
                        .get(inputVariableName)
                        .toRight(new VariableNotFoundException(inputVariableName, typeInfo._1))
      condition <- typeInfo._2.conditions.get(conditionName).toRight(new ConditionNotFoundException(conditionName))
      result    <- condition.check(inputVariable).left.map(new ServiceExecutionException(_))
    } yield {
      result
    }
  }
  def getOutput(variableName: String): Either[ExecutionRuntimeException, Rs] = this.outputGetter(variableName, runtime)

  private def updateVariable(rt: Map[String, TypeInfo], variable: Variable): Map[String, TypeInfo] = {
    val typeInfo         = rt.get(variable.typeName)
    val variables        = typeInfo.map(_.variables).getOrElse(Map.empty)
    val updatedVariable  = variables.get(variable.variableName).map(_.copy(value = variable.value)).getOrElse(variable)
    val updatedVariables = variables.updated(variable.variableName, updatedVariable)
    val updatedTypeInfo  = typeInfo.map(_.copy(variables = updatedVariables))
    updatedTypeInfo.map(t => rt.updated(variable.typeName, t)).getOrElse(rt)
  }
}
