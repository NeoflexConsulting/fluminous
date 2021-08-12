package org.fluminous.routing

sealed trait RoutingStep

sealed trait FirstStep extends RoutingStep

sealed trait IntermediateStep extends RoutingStep

final case class ExecuteFirstCondition(
  conditionName: String,
  ifTrueStep: IntermediateStep,
  ifFalseStep: IntermediateStep)
    extends FirstStep

final case class ExecuteFirstService(serviceName: String, outputVariableName: String, nextStep: IntermediateStep)
    extends FirstStep

final case class ExecuteCondition(
  conditionName: String,
  inputVariableName: String,
  ifTrueStep: IntermediateStep,
  ifFalseStep: IntermediateStep)
    extends IntermediateStep

final case class ExecuteService(
  serviceName: String,
  inputVariableName: String,
  outputVariableName: String,
  nextStep: IntermediateStep)
    extends IntermediateStep

final case class Finish(outputVariableName: String) extends IntermediateStep
