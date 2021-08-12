package org.fluminous.runtime

import org.fluminous.routing.IntermediateStep

case class RouterContext[Rs] (nextStep: IntermediateStep, er: ExecutionRuntime[Rs])
