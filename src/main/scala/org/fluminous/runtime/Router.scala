package org.fluminous.runtime

import io.circe.Json

trait Router[F[_], Rq, Rs] {
  def routeRequest(input: Rq): F[Rs]
  def untyped: Router[F, Json, Json]
}
