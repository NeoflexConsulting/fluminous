package org.fluminous.runtime

import org.fluminous.routing.IntermediateStep

case class RoutingContext[F[Monad], Rs](nextStep: IntermediateStep, er: ExecutionRuntime[F, Rs])
