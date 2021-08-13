package org.fluminous.runtime

import cats.Monad
import cats.data.EitherT
import org.fluminous.runtime.exception.{
  ConditionNotFoundException,
  ExecutionRuntimeException,
  InputValueNotFoundException,
  ServiceException,
  ServiceExecutionException,
  ServiceNotFoundException,
  VariableNotFoundException
}
import org.fluminous.services.RuntimeService

class ExecutionRuntime[F[_]: Monad, Rs](
  private val runtime: Map[String, TypeInfo[F]],
  private val outputGetter: (String, Map[String, TypeInfo[F]]) => Either[ExecutionRuntimeException, Rs]) {
  def executeFirstService(
    serviceName: String,
    outputVariableName: String
  ): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F, Rs]] = {
    for {
      serviceWithVariable      <- EitherT.fromEither[F](getServiceWithVariable(serviceName))
      (service, inputVariable) = serviceWithVariable
      result                   <- service.invoke(inputVariable, outputVariableName).leftMap(wrapServiceException)
    } yield { new ExecutionRuntime[F, Rs](updateVariable(runtime, result), outputGetter) }
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
  ): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F, Rs]] = {
    for {
      serviceWithVariable      <- EitherT.fromEither[F](getServiceWithVariable(serviceName, inputVariableName))
      (service, inputVariable) = serviceWithVariable
      result                   <- service.invoke(inputVariable, outputVariableName).leftMap(wrapServiceException)
    } yield { new ExecutionRuntime[F, Rs](updateVariable(runtime, result), outputGetter) }
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

  private def getServiceWithVariable(
    serviceName: String
  ): Either[ExecutionRuntimeException, (RuntimeService[F], Variable)] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.services.contains(serviceName))
                   .toRight(new ServiceNotFoundException(serviceName))
      inputValue    <- typeInfo._2.inputValue.toRight(new InputValueNotFoundException(typeInfo._2.typeName))
      inputVariable = Variable("input", typeInfo._1, inputValue)
      service       <- typeInfo._2.services.get(serviceName).toRight(new ServiceNotFoundException(serviceName))
    } yield (service, inputVariable)
  }

  private def getServiceWithVariable(
    serviceName: String,
    inputVariableName: String
  ): Either[ExecutionRuntimeException, (RuntimeService[F], Variable)] = {
    for {
      typeInfo <- runtime.toSeq
                   .find(_._2.services.contains(serviceName))
                   .toRight(new ServiceNotFoundException(serviceName))
      inputVariable <- typeInfo._2.variables
                        .get(inputVariableName)
                        .toRight(new VariableNotFoundException(inputVariableName, typeInfo._1))
      service <- typeInfo._2.services.get(serviceName).toRight(new ServiceNotFoundException(serviceName))
    } yield (service, inputVariable)
  }

  private def updateVariable(rt: Map[String, TypeInfo[F]], variable: Variable): Map[String, TypeInfo[F]] = {
    val typeInfo         = rt.get(variable.typeName)
    val variables        = typeInfo.map(_.variables).getOrElse(Map.empty)
    val updatedVariable  = variables.get(variable.variableName).map(_.copy(value = variable.value)).getOrElse(variable)
    val updatedVariables = variables.updated(variable.variableName, updatedVariable)
    val updatedTypeInfo  = typeInfo.map(_.copy(variables = updatedVariables))
    updatedTypeInfo.map(t => rt.updated(variable.typeName, t)).getOrElse(rt)
  }
  private def wrapServiceException(ex: ServiceException): ExecutionRuntimeException = {
    new ServiceExecutionException(ex)
  }
}
