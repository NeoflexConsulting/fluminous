package org.fluminous.routing

import cats.Monad
import cats.syntax.traverse._
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
  DuplicatedOperationId,
  ExpressionNotFound,
  InitialStateNotFound,
  InvalidOperation,
  InvalidStateReference,
  JqParserError,
  OpenAPIParsingError,
  OperationMissing,
  OperationNotFoundInOpenAPI,
  ServerNotFoundForDocument,
  UnsupportedStateType,
  WorkFlowBuildException
}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import cats.syntax.traverse._
import io.serverlessworkflow.api.functions.FunctionDefinition
import io.serverlessworkflow.api.workflow.Functions
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import org.fluminous.Settings
import org.fluminous.services.Service
import org.fluminous.services.rest.RestService

case class Routing[F[_]: Monad](firstStep: RoutingStep, services: Map[String, Service[F]])

object Routing extends Parser {
  def fromWorkflow[F[_]: Monad](settings: Settings, workflow: Workflow): Either[WorkFlowBuildException, Routing[F]] = {
    val states = workflow.getStates.asScala.toList
    for {
      services           <- getServices(settings, workflow.getFunctions)
      initialState       <- findInitialState(workflow)
      intermediateStates = states.filter(_.getName != initialState.getName)
      readySteps         <- buildSteps(intermediateStates, Right(Map.empty))
      initialState       <- buildStep(readySteps)(initialState)
    } yield Routing[F](initialState._2, services)
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
      case operationState: OperationStateAlg =>
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
      case operationState: OperationStateAlg =>
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
      state.getName -> Switch(state.getName, inputFilter, outputFilter, conditionFilter, ifTrueStep, ifFalseStep)
    }
  }

  private def buildStep(
    state: OperationStateAlg,
    readySteps: Map[String, RoutingStep]
  ): Either[WorkFlowBuildException, (String, RoutingStep)] = {
    for {
      actions      <- Option(state.getActions).flatMap(_.asScala).toList.traverse(readAction)
      inputFilter  <- asOption(state.getStateDataFilter)(_.getInput).map(extractFilter).getOrElse(Right(Root))
      outputFilter <- asOption(state.getStateDataFilter)(_.getOutput).map(extractFilter).getOrElse(Right(Root))
      nextState    <- getNextState(state, readySteps)
    } yield {
      state.getName -> Operation(
        state.getName,
        inputFilter,
        outputFilter,
        actions,
        nextState
      )
    }
  }

  private def readAction(action: Action): Either[WorkFlowBuildException, Invocation] = {
    for {
      arguments    <- readArguments(action)
      functionName = action.getFunctionRef.getRefName
      fromStateDataFilter <- asOption(action.getActionDataFilter)(_.getFromStateData)
                              .map(extractFilter)
                              .getOrElse(Right(Root))
      resultsFilter <- asOption(action.getActionDataFilter)(_.getResults).map(extractFilter).getOrElse(Right(Root))
      toStateDataFilter <- asOption(action.getActionDataFilter)(_.getToStateData)
                            .map(extractFilter)
                            .getOrElse(Right(Root))
    } yield {
      Invocation(functionName, arguments, fromStateDataFilter, resultsFilter, toStateDataFilter)
    }
  }

  private def readArguments(action: Action): Either[WorkFlowBuildException, Map[String, Filter]] = {
    val entries       = action.getFunctionRef.getArguments.fields().asScala.toList
    val arguments     = entries.map(entry => (entry.getKey, entry.getValue.asText()))
    val parsedFilters = arguments.map { case (name, value) => extractFilter(value).map(f => (name, f)) }.sequence
    parsedFilters.map(_.toMap)
  }

  private def getNextState(
    state: OperationStateAlg,
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

  private def isFinal(state: OperationStateAlg): Boolean = {
    state.getEnd != null
  }

  private def asOption[T, U](t: T)(f: T => U): Option[U] = {
    if (t != null) Option(f(t)) else None
  }

  private def getServices[F[_]: Monad](
    settings: Settings,
    functions: Functions
  ): Either[WorkFlowBuildException, Map[String, Service[F]]] = {
    functions.getFunctionDefs.asScala.toList.filter(isRest).traverse(getService[F](settings, _)).map(_.toMap)
  }

  private def getService[F[_]: Monad](
    settings: Settings,
    functionDef: FunctionDefinition
  ): Either[WorkFlowBuildException, (String, Service[F])] = {
    Option(functionDef.getOperation)
      .toRight(OperationMissing(functionDef.getName))
      .flatMap(getService[F](settings, _))
      .map(s => (s.name, s))
  }

  private def getService[F[_]: Monad](
    settings: Settings,
    operation: String
  ): Either[WorkFlowBuildException, Service[F]] = {
    operation.split("#").toList match {
      case document :: operationId :: Nil =>
        settings.servers
          .get(document)
          .toRight(ServerNotFoundForDocument(document))
          .flatMap(getService(_, document, operationId))
      case _ =>
        InvalidOperation(operation)
    }
  }

  private def getService[F[_]: Monad](
    server: String,
    document: String,
    operationId: String
  ): Either[WorkFlowBuildException, Service[F]] = {
    val resource = this.getClass.getClassLoader.getResource(document)
    val parser   = new OpenAPIParser().readLocation(resource.toString, null, null)
    Option(parser.getOpenAPI)
      .toRight(OpenAPIParsingError(document, parser.getMessages.asScala.toList))
      .flatMap(getService(server, document, operationId, _))
  }

  private def getService[F[_]: Monad](
    server: String,
    document: String,
    operationId: String,
    openAPI: OpenAPI
  ): Either[WorkFlowBuildException, Service[F]] = {
    val services = for {
      pathItem  <- openAPI.getPaths.asScala
      operation <- pathItem._2.readOperationsMap().asScala if operation._2.getOperationId == operationId
    } yield new RestService(server, pathItem._1, operation._1, operation._2)
    services.toList match {
      case Nil            => OperationNotFoundInOpenAPI(document, operationId)
      case service :: Nil => Right(service)
      case _ :: _ :: _    => DuplicatedOperationId(document, operationId)
    }
  }

  private def isRest(functionDef: FunctionDefinition): Boolean = {
    val functionType = Option(functionDef.getType)
    functionType.contains(FunctionDefinition.Type.REST) || functionType.isEmpty
  }
}
