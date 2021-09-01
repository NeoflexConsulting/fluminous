package org.fluminous.routing

import org.fluminous.jq.filter.Filter

sealed trait RoutingStep

final case class Switch(
  inputFilter: Filter,
  outputFilter: Filter,
  condition: Filter,
  ifTrueStep: RoutingStep,
  ifFalseStep: RoutingStep)
    extends RoutingStep

final case class Operation(
  inputFilter: Filter,
  outputFilter: Filter,
  functionName: String,
  arguments: Map[String, Filter],
  fromStateDataFilter: Filter,
  resultsFilter: Filter,
  toStateDataFilter: Filter,
  nextStep: RoutingStep)
    extends RoutingStep

case object Finish extends RoutingStep
