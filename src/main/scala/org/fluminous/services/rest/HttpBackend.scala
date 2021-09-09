package org.fluminous.services.rest

import sttp.capabilities. WebSockets
import sttp.client3.{ EitherBackend,SttpBackend }
import sttp.client3.okhttp.{ OkHttpFutureBackend, OkHttpSyncBackend }

import scala.concurrent.Future

trait HttpBackend[F[_]] {
  type P
  val backend: SttpBackend[F, P]
}

object HttpBackend {
  def apply[F[_]](implicit ev: HttpBackend[F]): HttpBackend[F] = ev

  implicit def eitherBackend = new HttpBackend[Either[Throwable, *]] {
    type P = WebSockets
    val backend = new EitherBackend(OkHttpSyncBackend())
  }

  implicit def futureBackend = new HttpBackend[Future] {
    type P = WebSockets
    val backend = OkHttpFutureBackend()
  }
}
