package org.fluminous.runtime

import io.circe.Json
import org.fluminous.runtime.exception.ExecutionRuntimeException

trait Router[F[_], Rq, Rs] {
  def routeRequest(input: Rq): F[Either[ExecutionRuntimeException, Rs]]
  def untyped: Router[F, Json, Json]
}
