package org.fluminous.routing

import io.serverlessworkflow.api.Workflow
import io.serverlessworkflow.api.interfaces.State
import io.serverlessworkflow.api.states.{ OperationState, SwitchState }
import org.fluminous.runtime.exception.WorkFlowBuildException

import scala.annotation.tailrec
import scala.collection.JavaConverters._

object Routing {
  def fromWorkflow(workflow: Workflow): Either[WorkFlowBuildException, FirstStep] = {
    val states = workflow.getStates.asScala.toList
    for {
      initialState       <- findInitialState(workflow)
      intermediateStates = states.filter(_.getName != initialState.getName)
      readySteps         <- buildIntermediateSteps(intermediateStates, Right(Map.empty))
      initialState       <- buildInitialStep(readySteps)(initialState)
    } yield initialState
  }

  @tailrec
  private def buildIntermediateSteps(
    states: List[State],
    readySteps: Either[WorkFlowBuildException, Map[String, IntermediateStep]]
  ): Either[WorkFlowBuildException, Map[String, IntermediateStep]] = {
    import cats.instances.either._, cats.syntax.traverse._
    readySteps match {
      case e @ Left(_) => e
      case Right(steps) =>
        val (readyForBuilding, notReadyForBuilding) = states.partition(couldBeBuilt(steps))
        val updatedReadySteps =
          readyForBuilding.map(buildIntermediateStep(steps)).sequence.map(b => b.toMap ++ steps)
        if (readyForBuilding.isEmpty) {
          Right(steps)
        } else {
          buildIntermediateSteps(notReadyForBuilding, updatedReadySteps)
        }
    }
  }

  private def couldBeBuilt(readySteps: Map[String, IntermediateStep])(state: State): Boolean = {
    state match {
      case operationState: OperationState =>
        isFinal(operationState) ||
          asOption(operationState.getTransition)(_.getNextState).exists(readySteps.contains)
      case switchState: SwitchState =>
        readySteps.contains(switchState.getDefault.getTransition.getNextState) &&
        switchState.getDataConditions.asScala.headOption.map(_.getTransition.getNextState).exists(readySteps.contains)
      case _ => false
    }
  }

  private def buildIntermediateStep(
    readySteps: Map[String, IntermediateStep]
  )(
    state: State
  ): Either[WorkFlowBuildException, (String, IntermediateStep)] = {
    state match {
      case operationState: OperationState =>
        buildIntermediateStep(operationState, readySteps)
      case switchState: SwitchState =>
        buildIntermediateStep(switchState, readySteps)
      case _ =>
        Left(new WorkFlowBuildException(s"Unsupported state type: ${state.getType.value()}"))
    }
  }

  private def buildIntermediateStep(
    state: SwitchState,
    readySteps: Map[String, IntermediateStep]
  ): Either[WorkFlowBuildException, (String, IntermediateStep)] = {
    for {
      condition <- state.getDataConditions.asScala.headOption
                    .toRight(new WorkFlowBuildException(s"Condition in state ${state.getName} not found "))
      conditionName     <- extractVariableName(condition.getCondition)
      inputVariableName <- extractVariableName(state.getStateDataFilter.getInput)
      ifTrueStep <- readySteps
                     .get(condition.getTransition.getNextState)
                     .toRight(new WorkFlowBuildException(s"state ${state.getName} references to invalid next state"))
      ifFalseStep <- readySteps
                      .get(state.getDefault.getTransition.getNextState)
                      .toRight(new WorkFlowBuildException(s"state ${state.getName} references to invalid next state"))
    } yield {
      state.getName -> ExecuteCondition(conditionName, inputVariableName, ifTrueStep, ifFalseStep)
    }
  }

  private def buildIntermediateStep(
    state: OperationState,
    readySteps: Map[String, IntermediateStep]
  ): Either[WorkFlowBuildException, (String, IntermediateStep)] = {
    for {
      action <- state.getActions.asScala.headOption
                 .toRight(new WorkFlowBuildException(s"Action in state ${state.getName} not found "))
      serviceName        = action.getFunctionRef.getRefName
      inputVariableName  <- extractVariableName(action.getActionDataFilter.getFromStateData)
      outputVariableName <- extractVariableName(action.getActionDataFilter.getToStateData)
      nextState          <- getNextState(state, outputVariableName, readySteps)
    } yield {
      state.getName -> ExecuteService(serviceName, inputVariableName, outputVariableName, nextState)
    }
  }

  private def buildInitialStep(
    readySteps: Map[String, IntermediateStep]
  )(
    state: State
  ): Either[WorkFlowBuildException, FirstStep] = {
    state match {
      case operationState: OperationState =>
        buildInitialStep(operationState, readySteps)
      case switchState: SwitchState =>
        buildInitialStep(switchState, readySteps)
      case _ =>
        Left(new WorkFlowBuildException(s"Unsupported state type: ${state.getType.value()}"))
    }
  }

  private def buildInitialStep(
    state: SwitchState,
    readySteps: Map[String, IntermediateStep]
  ): Either[WorkFlowBuildException, FirstStep] = {
    for {
      condition <- state.getDataConditions.asScala.headOption
                    .toRight(new WorkFlowBuildException(s"Condition in state ${state.getName} not found "))
      conditionName <- extractVariableName(condition.getCondition)
      ifTrueStep <- readySteps
                     .get(condition.getTransition.getNextState)
                     .toRight(new WorkFlowBuildException(s"state ${state.getName} references to invalid next state"))
      ifFalseStep <- readySteps
                      .get(state.getDefault.getTransition.getNextState)
                      .toRight(new WorkFlowBuildException(s"state ${state.getName} references to invalid next state"))
    } yield {
      ExecuteFirstCondition(conditionName, ifTrueStep, ifFalseStep)
    }
  }

  private def buildInitialStep(
    state: OperationState,
    readySteps: Map[String, IntermediateStep]
  ): Either[WorkFlowBuildException, FirstStep] = {
    for {
      action <- state.getActions.asScala.headOption
                 .toRight(new WorkFlowBuildException(s"Action in state ${state.getName} not found "))
      serviceName        = action.getFunctionRef.getRefName
      outputVariableName <- extractVariableName(action.getActionDataFilter.getToStateData)
      nextState          <- getNextState(state, outputVariableName, readySteps)
    } yield {
      ExecuteFirstService(serviceName, outputVariableName, nextState)
    }
  }

  private def getNextState(
    state: OperationState,
    outputVariableName: String,
    readySteps: Map[String, IntermediateStep]
  ): Either[WorkFlowBuildException, IntermediateStep] = {
    if (isFinal(state))
      Right(Finish(outputVariableName))
    else
      readySteps
        .get(state.getTransition.getNextState)
        .toRight(new WorkFlowBuildException(s"state ${state.getName} references to invalid next state"))
  }

  private def findInitialState(workFlow: Workflow): Either[WorkFlowBuildException, State] = {
    workFlow.getStates.asScala
      .find(_.getName == workFlow.getStart.getStateName)
      .toRight(new WorkFlowBuildException(s"Initial state not found"))
  }

  private def extractVariableName(expression: String): Either[WorkFlowBuildException, String] = {
    val matcher = variablePattern.pattern.matcher(expression)
    if (matcher.matches()) {
      Right(matcher.group(1))
    } else {
      Left(new WorkFlowBuildException(s"Expression $expression does not conform to variable specification"))
    }
  }

  private def isFinal(state: OperationState): Boolean = {
    state.getEnd != null
  }

  private def asOption[T, U](t: T)(f: T => U): Option[U] = {
    if (t != null) Some(f(t)) else None
  }

  private val variablePattern = raw"""\$$\{([a-zA-Z]\w*)\}""".r
}
