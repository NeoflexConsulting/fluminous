package org.fluminous.services.rest

import cats.effect.{ ExitCode, IO, IOApp }
import org.fluminous.services.rest.model.Customer
import org.http4s._
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import io.circe.generic.auto._
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import org.typelevel.log4cats.SelfAwareStructuredLogger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object TestServer extends TestServerT

trait TestServerT extends IOApp.Simple {
  def customerService(logger: SelfAwareStructuredLogger[IO]): HttpApp[IO] =
    HttpRoutes
      .of[IO] {
        case GET -> Root / "customers" / IntVar(customerId) =>
          for {
            _    <- logger.warn(s"GetCustomer for customerId: $customerId")
            resp <- Ok(Customer(customerId, "Ivan", 24))
          } yield resp

        case req @ PUT -> Root / "customers" / IntVar(customerId) =>
          for {
            customer <- req.as[Customer]
            _        <- logger.warn(s"UpdateCustomer for customerId: $customerId with customer: $customer")
            resp     <- Ok()
          } yield resp
      }
      .orNotFound

  val run: IO[Unit] = {
    Slf4jLogger.create[IO].flatMap { logger =>
      BlazeServerBuilder[IO]
        .bindHttp(8080, "localhost")
        .withHttpApp(customerService(logger))
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }

  }
}
