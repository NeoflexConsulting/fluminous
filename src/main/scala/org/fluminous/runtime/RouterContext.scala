package org.fluminous.runtime

import org.fluminous.routing.IntermediateStep

case class RouterContext[F[Monad], Rs](nextStep: IntermediateStep, er: ExecutionRuntime[F, Rs])
