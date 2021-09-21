package org.fluminous.routing

import cats.{ Monad, MonadThrow }
import io.serverlessworkflow.api.Workflow
import io.serverlessworkflow.api.actions.Action
import io.serverlessworkflow.api.interfaces.State
import io.serverlessworkflow.api.states.{ OperationState, SwitchState }
import org.fluminous.jq.Parser
import org.fluminous.jq.filter.{ Filter, Identity }

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import cats.syntax.traverse._
import io.circe.Json
import io.serverlessworkflow.api.functions.FunctionDefinition
import io.serverlessworkflow.api.workflow.Functions
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.{ OpenAPI, Operation }
import io.swagger.v3.oas.models.PathItem.HttpMethod
import org.fluminous.Settings
import org.fluminous.runtime.{ ActionExecutor, OperationExecutor, SwitchExecutor }
import org.fluminous.services.Service
import org.fluminous.services.rest.{
  DeleteService,
  EndpointTemplate,
  GetService,
  HttpBackend,
  PostService,
  PutService,
  RestService
}

class RoutingBuilder[F[_]: MonadThrow: HttpBackend](builtInServices: Map[String, Service[F]], settings: Settings)
    extends Parser
    with ParameterFunctions {
  type Result[T] = Either[WorkFlowBuildException, T]
  def fromWorkflow(workflow: Workflow): Result[Json => F[Json]] = {
    val states = workflow.getStates.asScala.toList
    for {
      configuredServices <- getServices(workflow.getFunctions)
      services           = builtInServices ++ configuredServices
      initialState       <- findInitialState(workflow)
      intermediateStates = states.filter(_.getName != initialState.getName)
      readySteps         <- buildSteps(intermediateStates, services, Right(Map.empty))
      initialState       <- buildStep(services, readySteps)(initialState)
    } yield initialState._2
  }

  @tailrec
  private def buildSteps(
    states: List[State],
    services: Map[String, Service[F]],
    readySteps: Result[Map[String, Json => F[Json]]]
  ): Result[Map[String, Json => F[Json]]] = {
    import cats.instances.either._, cats.syntax.traverse._
    readySteps match {
      case e @ Left(_) => e
      case Right(steps) =>
        val (readyForBuilding, notReadyForBuilding) = states.partition(couldBeBuilt(steps))
        val updatedReadySteps =
          readyForBuilding.map(buildStep(services, steps)).sequence.map(b => b.toMap ++ steps)
        if (readyForBuilding.isEmpty) {
          Right(steps)
        } else {
          buildSteps(notReadyForBuilding, services, updatedReadySteps)
        }
    }
  }

  private def couldBeBuilt(readySteps: Map[String, Json => F[Json]])(state: State): Boolean = {
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
    services: Map[String, Service[F]],
    readySteps: Map[String, Json => F[Json]]
  )(
    state: State
  ): Result[(String, Json => F[Json])] = {
    state match {
      case operationState: OperationState =>
        buildStep(operationState, services, readySteps)
      case switchState: SwitchState =>
        buildStep(switchState, readySteps)
      case _ =>
        UnsupportedStateType(state.getType.value())
    }
  }

  private def buildStep(
    state: SwitchState,
    readySteps: Map[String, Json => F[Json]]
  ): Result[(String, Json => F[Json])] = {
    for {

      condition           <- state.getDataConditions.asScala.headOption.toRight(ConditionNotFound(state.getName))
      conditionExpression <- Option(condition.getCondition).toRight(ConditionNotFound(state.getName))
      conditionFilter     <- extractFilter(conditionExpression)
      inputFilter         <- asOption(state.getStateDataFilter)(_.getInput).map(extractFilter).getOrElse(Right(Identity))
      outputFilter <- asOption(state.getStateDataFilter)(_.getOutput)
                       .map(extractFilter)
                       .getOrElse(Right(Identity))
      ifTrueStep <- readySteps
                     .get(condition.getTransition.getNextState)
                     .toRight(InvalidStateReference(state.getName, condition.getTransition.getNextState))
      ifFalseStep <- readySteps
                      .get(state.getDefault.getTransition.getNextState)
                      .toRight(InvalidStateReference(state.getName, state.getDefault.getTransition.getNextState))
    } yield {
      state.getName -> SwitchExecutor(state.getName, inputFilter, outputFilter, conditionFilter)
        .execute(ifTrueStep, ifFalseStep) _
    }
  }

  private def buildStep(
    state: OperationState,
    services: Map[String, Service[F]],
    readySteps: Map[String, Json => F[Json]]
  ): Result[(String, Json => F[Json])] = {
    for {
      actions     <- Option(state.getActions).map(_.asScala.toList).toList.flatten.traverse(readAction(_, services))
      inputFilter <- asOption(state.getStateDataFilter)(_.getInput).map(extractFilter).getOrElse(Right(Identity))
      outputFilter <- asOption(state.getStateDataFilter)(_.getOutput)
                       .map(extractFilter)
                       .getOrElse(Right(Identity))
      nextStep <- getNextStep(state, readySteps)
    } yield {
      state.getName -> OperationExecutor(state.getName, inputFilter, outputFilter).execute(actions, nextStep) _
    }
  }

  private def readAction(action: Action, services: Map[String, Service[F]]): Result[Json => F[Json]] = {
    for {
      arguments    <- readArguments(action)
      functionName = action.getFunctionRef.getRefName
      service      <- services.get(functionName).toRight(ServiceNotFoundException(functionName))
      fromStateDataFilter <- asOption(action.getActionDataFilter)(_.getFromStateData)
                              .map(extractFilter)
                              .getOrElse(Right(Identity))
      resultsFilter <- asOption(action.getActionDataFilter)(_.getResults)
                        .map(extractFilter)
                        .getOrElse(Right(Identity))
      toStateDataFilter <- asOption(action.getActionDataFilter)(_.getToStateData)
                            .map(extractFilter)
                            .getOrElse(Right(Identity))
    } yield {
      ActionExecutor(arguments, fromStateDataFilter, resultsFilter, toStateDataFilter).execute(service) _
    }
  }

  private def readArguments(action: Action): Result[List[(String, Filter)]] = {
    val entries       = action.getFunctionRef.getArguments.fields().asScala.toList
    val arguments     = entries.map(entry => (entry.getKey, entry.getValue.asText()))
    val parsedFilters = arguments.map { case (name, value) => extractFilter(value).map(f => (name, f)) }.sequence
    parsedFilters
  }

  private def getNextStep(state: OperationState, readySteps: Map[String, Json => F[Json]]): Result[Json => F[Json]] = {
    if (isFinal(state))
      Right(Monad[F].pure(_))
    else
      readySteps
        .get(state.getTransition.getNextState)
        .toRight(InvalidStateReference(state.getName, state.getTransition.getNextState))
  }

  private def findInitialState(workFlow: Workflow): Result[State] = {
    workFlow.getStates.asScala
      .find(_.getName == workFlow.getStart.getStateName)
      .toRight(InitialStateNotFound())
  }

  private def extractFilter(expression: String): Result[Filter] = {
    if (!expression.startsWith("${") || !expression.endsWith("}")) {
      Left(ExpressionNotFound(expression))
    } else {
      parse(expression.substring(2).dropRight(1)).left.map(JqParserError(expression, _))
    }
  }

  private def isFinal(state: OperationState): Boolean = {
    state.getEnd != null
  }

  private def asOption[T, U](t: T)(f: T => U): Option[U] = {
    if (t != null) Option(f(t)) else None
  }

  private def getServices(functions: Functions): Result[Map[String, Service[F]]] = {
    functions.getFunctionDefs.asScala.toList.filter(isRest).traverse(getService).map(_.toMap)
  }

  private def getService(functionDef: FunctionDefinition): Result[(String, Service[F])] = {
    Option(functionDef.getOperation)
      .toRight(OperationMissing(functionDef.getName))
      .flatMap(getService)
      .map(s => (s.name, s))
  }

  private def getService(operation: String): Result[Service[F]] = {
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

  private def getService(server: String, document: String, operationId: String): Result[Service[F]] = {
    val resource = this.getClass.getClassLoader.getResource(document)
    val parser   = new OpenAPIParser().readLocation(resource.toString, null, null)
    Option(parser.getOpenAPI)
      .toRight(OpenAPIParsingError(document, parser.getMessages.asScala.toList))
      .flatMap(getService(server, document, operationId, _))
  }

  private def getService(
    server: String,
    document: String,
    operationId: String,
    openAPI: OpenAPI
  ): Result[Service[F]] = {
    val services = for {
      pathItem  <- openAPI.getPaths.asScala
      operation <- pathItem._2.readOperationsMap().asScala if operation._2.getOperationId == operationId
    } yield getService(server, pathItem._1, operation._1, operation._2)
    services.toList match {
      case Nil                   => OperationNotFoundInOpenAPI(document, operationId)
      case Right(service) :: Nil => Right(service)
      case Left(ex) :: Nil       => Left(ex)
      case _ :: _ :: _           => DuplicatedOperationId(document, operationId)
    }
  }

  private def getService(
    server: String,
    path: String,
    method: HttpMethod,
    operation: Operation
  ): Result[RestService[F]] = {
    val parameters         = operation.getParameters.asScala
    val pathParameters     = parameters.filter(isPath).toList
    val queryParameters    = parameters.filter(isQuery).toList
    val headerParameters   = parameters.filter(isHeader).toList
    val headerRequirements = headerParameters.map(p => (p.getName, isRequired(p)))
    for {
      endpointTemplate <- EndpointTemplate(server, operation.getOperationId, path, pathParameters, queryParameters)
      service <- method match {
                  case HttpMethod.POST =>
                    Right(new PostService[F](operation.getOperationId, endpointTemplate, headerRequirements))
                  case HttpMethod.PUT =>
                    Right(new PutService[F](operation.getOperationId, endpointTemplate, headerRequirements))
                  case HttpMethod.GET =>
                    Right(new GetService[F](operation.getOperationId, endpointTemplate, headerRequirements))
                  case HttpMethod.DELETE =>
                    Right(new DeleteService[F](operation.getOperationId, endpointTemplate, headerRequirements))
                  case _ => Left(UnsupportedHttpMethod(operation.getOperationId, method.name()))
                }
    } yield service
  }

  private def isRest(functionDef: FunctionDefinition): Boolean = {
    val functionType = Option(functionDef.getType)
    functionType.contains(FunctionDefinition.Type.REST) || functionType.isEmpty
  }
}
