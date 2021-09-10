package org.fluminous

import org.fluminous.model.{ ChangeAgeRq, ChangeAgeRs, Customer }
import org.fluminous.services.ServiceCollection

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.Source

object TestAsyncExample {
  def main(args: Array[String]): Unit = {
    testRouting
  }
  private def testRestRouting = {
    import io.circe.generic.auto._
    //Filling service collection
    val serviceCollection =
      ServiceCollection[Future]()
        .addSyncService[Int, Int, Boolean]("isSame", _ == _, "input1", "input2")
    val json     = Source.fromResource("RESTRouting.json").mkString
    val settings = Settings(Map("CustomerService.json" -> "localhost:8080"))
    serviceCollection
      .toRouter[ChangeAgeRq, ChangeAgeRs](json, settings)
      .fold(
        printErrorInfo, { router =>
          val resultFuture1 = router.routeRequest(ChangeAgeRq(123243, 25))
          println("Awaiting result1.....")
          val result1 = Await.result(resultFuture1, 20 seconds)
          println(s"Result1 arrived: $result1")

          val resultFuture2 = router.routeRequest(ChangeAgeRq(123243, 24))
          println("Awaiting result2.....")
          val result2 = Await.result(resultFuture2, 20 seconds)
          println(s"Result2 arrived: $result2")
        }
      )
  }

  private def testRouting = {
    import io.circe.generic.auto._
    val serviceCollection =
      ServiceCollection[Future]()
        .addService[String, String]("upper", wrapToAsync(_.toUpperCase), "input")
        .addService[Int, Int]("increment", wrapToAsync(_ + 1), "input")
        .addService[Int, String]("to_string", wrapToAsync(_.toString), "input")
        .addService[String, Int]("to_int", wrapToAsync(_.toInt), "input")
        .addService[Customer, Int]("get_age", wrapToAsync(_.age), "input")
        .addService[Customer, String]("get_name", wrapToAsync(_.name), "input")
        .addService[Customer, Customer]("increase_age", wrapToAsync(c => c.copy(age = c.age + 1)), "input")
        .addService[Int, Customer]("get_customer_by_age", wrapToAsync(age => Customer("testCustomer", age)), "input")
        .addService[String, Customer]("get_customer_by_name", wrapToAsync(name => Customer(name, 25)), "input")
        .addService[String, Boolean]("is_number", wrapToAsync(_.forall(_.isDigit)), "input")

    val json     = Source.fromResource("Routing.json").mkString
    val settings = Settings(Map("CustomerService.json" -> "localhost:8080"))
    serviceCollection
      .toRouter[String, Customer](json, settings)
      .fold(
        printErrorInfo, { router =>
          val resultFuture1 = router.routeRequest("Иван")
          println("Awaiting result1.....")
          val result1 = Await.result(resultFuture1, 20 seconds)
          println(s"Result1 arrived: $result1")

          val resultFuture2 = router.routeRequest("12")
          println("Awaiting result2.....")
          val result2 = Await.result(resultFuture2, 20 seconds)
          println(s"Result2 arrived: $result2")
        }
      )
  }

  def wrapToAsync[A, B](func: A => B): A => Future[B] = { a =>
    Future {
      Thread.sleep(2000)
      func(a)
    }
  }

  @tailrec
  def printErrorInfo(ex: Throwable): Unit = {
    println(ex.getClass)
    println(ex.getMessage)
    ex.printStackTrace(System.out)
    if (ex.getCause != null) {
      println("Caused by: ")
      printErrorInfo(ex.getCause)
    }
  }

}
