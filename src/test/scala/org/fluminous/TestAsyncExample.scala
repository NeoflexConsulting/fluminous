package org.fluminous

import io.serverlessworkflow.api.workflow.BaseWorkflow
import org.fluminous.model.Customer
import org.fluminous.routing.Routing
import org.fluminous.services.{ AsyncService, ServiceCollection }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }
import scala.io.Source

object TestAsyncExample {
  def main(args: Array[String]): Unit = {
    import io.circe.generic.auto._
    val upperCaseService = AsyncService[String, String]("upper", wrapToAsync(_.toUpperCase), "input")
    val incrementService = AsyncService[Int, Int]("increment", wrapToAsync(_ + 1), "input")
    val toStringService  = AsyncService[Int, String]("to_string", wrapToAsync(_.toString), "input")
    val toIntService     = AsyncService[String, Int]("to_int", wrapToAsync(_.toInt), "input")
    val getAgeService    = AsyncService[Customer, Int]("get_age", wrapToAsync(_.age), "input")
    val getNameService   = AsyncService[Customer, String]("get_name", wrapToAsync(_.name), "input")
    val increaseAgeService =
      AsyncService[Customer, Customer]("increase_age", wrapToAsync(c => c.copy(age = c.age + 1)), "input")
    val getCustomerByAgeService =
      AsyncService[Int, Customer]("get_customer_by_age", wrapToAsync(age => Customer("testCustomer", age)), "input")
    val getCustomerByNameService =
      AsyncService[String, Customer]("get_customer_by_name", wrapToAsync(name => Customer(name, 25)), "input")
    val isNumber = AsyncService[String, Boolean]("is_number", wrapToAsync(_.forall(_.isDigit)), "input")

    val serviceCollection =
      ServiceCollection[Future]()
        .addService(upperCaseService)
        .addService(incrementService)
        .addService(toStringService)
        .addService(toIntService)
        .addService(getAgeService)
        .addService(getNameService)
        .addService(increaseAgeService)
        .addService(getCustomerByAgeService)
        .addService(getCustomerByNameService)
        .addService(isNumber)

    val json     = Source.fromResource("Routing.json").mkString
    val workflow = BaseWorkflow.fromSource(json)
    val settings = Settings(Map("CustomerService.json" -> "localhost"))
    val routing  = Routing.fromWorkflow[Future](settings, workflow)
    //Creating router
    routing.foreach { r =>
      val router = serviceCollection.toRouter[String, Customer](r)

      val resultFuture1 = router.routeRequest("Иван")
      println("Awaiting result1.....")
      val result1 = Await.result(resultFuture1, 60 seconds)
      println(s"Result1 arrived: $result1")

      val resultFuture2 = router.routeRequest("12")
      println("Awaiting result2.....")
      val result2 = Await.result(resultFuture2, 60 seconds)
      println(s"Result2 arrived: $result2")
    }
  }

  def wrapToAsync[A, B](func: A => B): A => Future[B] = { a =>
    Future {
      Thread.sleep(10000)
      func(a)
    }
  }
}
