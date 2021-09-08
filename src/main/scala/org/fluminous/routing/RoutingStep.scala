package org.fluminous.routing

import cats.Monad
import cats.data.EitherT
import org.fluminous.jq.filter.Filter
import org.fluminous.runtime.ExecutionRuntime
import org.fluminous.runtime.exception.ExecutionRuntimeException

sealed trait RoutingStep

trait ExecutionStep extends RoutingStep {
  def executeInRuntime[F[_]: Monad](
    runtime: ExecutionRuntime[F]
  ): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F]]
}

final case class Switch(
  stateName: String,
  inputFilter: Filter,
  outputFilter: Filter,
  condition: Filter,
  ifTrueStep: RoutingStep,
  ifFalseStep: RoutingStep)
    extends ExecutionStep {
  override def executeInRuntime[F[_]: Monad](
    runtime: ExecutionRuntime[F]
  ): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F]] = {
    EitherT.fromEither[F](runtime.executeSwitch(this))
  }
}

final case class Operation(
  stateName: String,
  inputFilter: Filter,
  outputFilter: Filter,
  actions: Seq[Invocation],
  nextStep: RoutingStep)
    extends ExecutionStep {
  override def executeInRuntime[F[_]: Monad](
    runtime: ExecutionRuntime[F]
  ): EitherT[F, ExecutionRuntimeException, ExecutionRuntime[F]] = {
    runtime.executeOperation(this)
  }
}

case class Invocation(
  functionName: String,
  arguments: Map[String, Filter],
  fromStateDataFilter: Filter,
  resultsFilter: Filter,
  toStateDataFilter: Filter)

case object Finish extends RoutingStep
