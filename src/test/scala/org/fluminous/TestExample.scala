package org.fluminous

import org.fluminous.model.{ ChangeAgeRq, ChangeAgeRs, Customer }
import org.fluminous.services.ServiceCollection

import scala.annotation.tailrec
import scala.io.Source

object TestExample {
  def main(args: Array[String]): Unit = {
    testRouting
    testRestRouting
  }

  private def testRestRouting = {
    import io.circe.generic.auto._
    val serviceCollection =
      ServiceCollection[Either[Throwable, *]]()
        .addSyncService[Int, Int, Boolean]("isSame", _ == _, "input1", "input2")
    val json     = Source.fromResource("RESTRouting.json").mkString
    val settings = Settings(Map("CustomerService.json" -> "localhost:8080"))
    serviceCollection
      .toRouter[ChangeAgeRq, ChangeAgeRs](json, settings)
      .fold(
        printErrorInfo, { router =>
          val result1 = router.routeRequest(ChangeAgeRq(123243, 25))
          result1.fold(printErrorInfo, println)
          val result2 = router.routeRequest(ChangeAgeRq(123243, 24))
          result2.fold(printErrorInfo, println)
        }
      )
  }

  private def testRouting = {
    import io.circe.generic.auto._
    val serviceCollection =
      ServiceCollection[Either[Throwable, *]]()
        .addSyncService[String, String]("upper", _.toUpperCase, "input")
        .addSyncService[Int, Int]("increment", _ + 1, "input")
        .addSyncService[Int, String]("to_string", _.toString, "input")
        .addSyncService[String, Int]("to_int", _.toInt, "input")
        .addSyncService[Customer, Int]("get_age", _.age, "input")
        .addSyncService[Customer, String]("get_name", _.name, "input")
        .addSyncService[Customer, Customer]("increase_age", c => c.copy(age = c.age + 1), "input")
        .addSyncService[Int, Customer]("get_customer_by_age", age => Customer("testCustomer", age), "input")
        .addSyncService[String, Customer]("get_customer_by_name", name => Customer(name, 25), "input")
        .addSyncService[String, Boolean]("is_number", _.forall(_.isDigit), "input")

    val json     = Source.fromResource("Routing.json").mkString
    val settings = Settings(Map("CustomerService.json" -> "localhost:8080"))
    serviceCollection
      .toRouter[String, Customer](json, settings)
      .fold(
        printErrorInfo, { router =>
          val result1 = router.routeRequest("Иван")
          result1.fold(printErrorInfo, println)
          val result2 = router.routeRequest("12")
          result2.fold(printErrorInfo, println)
        }
      )
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
