package org.fluminous.routing

import io.serverlessworkflow.api.Workflow
import io.serverlessworkflow.api.actions.Action
import io.serverlessworkflow.api.interfaces.State
import io.serverlessworkflow.api.states.{ OperationState, SwitchState }
import org.fluminous.jq.Parser
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.tokens.Root
import org.fluminous.runtime.exception.{
  ActionNotFound,
  ConditionNotFound,
  ExpressionNotFound,
  InitialStateNotFound,
  InvalidStateReference,
  JqParserError,
  UnsupportedStateType,
  WorkFlowBuildException
}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import cats.syntax.traverse._

object Routing extends Parser {
  def fromWorkflow(workflow: Workflow): Either[WorkFlowBuildException, RoutingStep] = {
    val states = workflow.getStates.asScala.toList
    for {
      initialState       <- findInitialState(workflow)
      intermediateStates = states.filter(_.getName != initialState.getName)
      readySteps         <- buildSteps(intermediateStates, Right(Map.empty))
      initialState       <- buildStep(readySteps)(initialState)
    } yield initialState._2
  }

  @tailrec
  private def buildSteps(
    states: List[State],
    readySteps: Either[WorkFlowBuildException, Map[String, RoutingStep]]
  ): Either[WorkFlowBuildException, Map[String, RoutingStep]] = {
    import cats.instances.either._, cats.syntax.traverse._
    readySteps match {
      case e @ Left(_) => e
      case Right(steps) =>
        val (readyForBuilding, notReadyForBuilding) = states.partition(couldBeBuilt(steps))
        val updatedReadySteps =
          readyForBuilding.map(buildStep(steps)).sequence.map(b => b.toMap ++ steps)
        if (readyForBuilding.isEmpty) {
          Right(steps)
        } else {
          buildSteps(notReadyForBuilding, updatedReadySteps)
        }
    }
  }

  private def couldBeBuilt(readySteps: Map[String, RoutingStep])(state: State): Boolean = {
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

  private def buildStep(
    readySteps: Map[String, RoutingStep]
  )(
    state: State
  ): Either[WorkFlowBuildException, (String, RoutingStep)] = {
    state match {
      case operationState: OperationState =>
        buildStep(operationState, readySteps)
      case switchState: SwitchState =>
        buildStep(switchState, readySteps)
      case _ =>
        UnsupportedStateType(state.getType.value())
    }
  }

  private def buildStep(
    state: SwitchState,
    readySteps: Map[String, RoutingStep]
  ): Either[WorkFlowBuildException, (String, RoutingStep)] = {
    for {

      condition           <- state.getDataConditions.asScala.headOption.toRight(ConditionNotFound(state.getName))
      conditionExpression <- Option(condition.getCondition).toRight(ConditionNotFound(state.getName))
      conditionFilter     <- extractFilter(conditionExpression)
      inputFilter         <- asOption(state.getStateDataFilter)(_.getInput).map(extractFilter).getOrElse(Right(Root))
      outputFilter        <- asOption(state.getStateDataFilter)(_.getOutput).map(extractFilter).getOrElse(Right(Root))
      ifTrueStep <- readySteps
                     .get(condition.getTransition.getNextState)
                     .toRight(InvalidStateReference(state.getName, condition.getTransition.getNextState))
      ifFalseStep <- readySteps
                      .get(state.getDefault.getTransition.getNextState)
                      .toRight(InvalidStateReference(state.getName, state.getDefault.getTransition.getNextState))
    } yield {
      state.getName -> Switch(inputFilter, outputFilter, conditionFilter, ifTrueStep, ifFalseStep)
    }
  }

  private def buildStep(
    state: OperationState,
    readySteps: Map[String, RoutingStep]
  ): Either[WorkFlowBuildException, (String, RoutingStep)] = {
    for {
      action       <- state.getActions.asScala.headOption.toRight(ActionNotFound(state.getName))
      functionName = action.getFunctionRef.getRefName
      arguments    <- readArguments(action)
      inputFilter  <- asOption(state.getStateDataFilter)(_.getInput).map(extractFilter).getOrElse(Right(Root))
      outputFilter <- asOption(state.getStateDataFilter)(_.getOutput).map(extractFilter).getOrElse(Right(Root))
      fromStateDataFilter <- asOption(action.getActionDataFilter)(_.getFromStateData)
                              .map(extractFilter)
                              .getOrElse(Right(Root))
      resultsFilter <- asOption(action.getActionDataFilter)(_.getResults).map(extractFilter).getOrElse(Right(Root))
      toStateDataFilter <- asOption(action.getActionDataFilter)(_.getToStateData)
                            .map(extractFilter)
                            .getOrElse(Right(Root))
      nextState <- getNextState(state, readySteps)
    } yield {
      state.getName -> Operation(
        inputFilter,
        outputFilter,
        functionName,
        arguments,
        fromStateDataFilter,
        resultsFilter,
        toStateDataFilter,
        nextState
      )
    }
  }

  private def readArguments(action: Action): Either[WorkFlowBuildException, Map[String, Filter]] = {
    val entries       = action.getFunctionRef.getArguments.fields().asScala.toList
    val arguments     = entries.map(entry => (entry.getKey, entry.getValue.asText()))
    val parsedFilters = arguments.map { case (name, value) => parse(value).map(f => (name, f)) }.sequence
    parsedFilters.map(_.toMap).left.map(JqParserError)
  }

  private def getNextState(
    state: OperationState,
    readySteps: Map[String, RoutingStep]
  ): Either[WorkFlowBuildException, RoutingStep] = {
    if (isFinal(state))
      Right(Finish)
    else
      readySteps
        .get(state.getTransition.getNextState)
        .toRight(InvalidStateReference(state.getName, state.getTransition.getNextState))
  }

  private def findInitialState(workFlow: Workflow): Either[WorkFlowBuildException, State] = {
    workFlow.getStates.asScala
      .find(_.getName == workFlow.getStart.getStateName)
      .toRight(InitialStateNotFound())
  }

  private def extractFilter(expression: String): Either[WorkFlowBuildException, Filter] = {
    if (!expression.startsWith("${") || !expression.endsWith("}")) {
      Left(ExpressionNotFound(expression))
    } else {
      parse(expression.substring(2).dropRight(1)).left.map(JqParserError)
    }
  }

  private def isFinal(state: OperationState): Boolean = {
    state.getEnd != null
  }

  private def asOption[T, U](t: T)(f: T => U): Option[U] = {
    if (t != null) Some(f(t)) else None
  }
}
