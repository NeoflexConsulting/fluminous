package org.fluminous

import org.fluminous.routing.{ ExecuteFirstService, ExecuteService, Finish }
import org.fluminous.services.{ AsyncService, ServiceCollection }

import scala.concurrent.{ Await, Future }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object TestAsyncExample {
  def main(args: Array[String]): Unit = {
    val upperCaseService   = AsyncService[String, String]("upper", wrapToAsync(_.toUpperCase))
    val incrementService   = AsyncService[Int, Int]("increment", wrapToAsync(_ + 1))
    val toStringService    = AsyncService[Int, String]("to_string", wrapToAsync(_.toString))
    val toIntService       = AsyncService[String, Int]("to_int", wrapToAsync(_.toInt))
    val getAgeService      = AsyncService[Customer, Int]("get_age", wrapToAsync(_.age))
    val getNameService     = AsyncService[Customer, String]("get_name", wrapToAsync(_.name))
    val increaseAgeService = AsyncService[Customer, Customer]("increase_age", wrapToAsync(c => c.copy(age = c.age + 1)))
    val getCustomerByAgeService =
      AsyncService[Int, Customer]("get_customer_by_age", wrapToAsync(age => Customer("testCustomer", age)))
    val getCustomerByNameService =
      AsyncService[String, Customer]("get_customer_by_name", wrapToAsync(name => Customer(name, 25)))

    val serviceCollection =
      ServiceCollection[Future]()
        .addType[Int]
        .addType[String]
        .addType[Customer]
        .addService(upperCaseService)
        .addService(incrementService)
        .addService(toStringService)
        .addService(toIntService)
        .addService(getAgeService)
        .addService(getNameService)
        .addService(increaseAgeService)
        .addService(getCustomerByAgeService)
        .addService(getCustomerByNameService)

    //Routing information
    val routing = ExecuteFirstService(
      "upper",
      "upperName",
      ExecuteService("get_customer_by_name", "upperName", "result", Finish("result"))
    )
    //Creating router
    val router = serviceCollection.toRouter[String, Customer]

    val resultFuture = router.routeRequest("Иван", routing)

    println("Awaiting result.....")
    val result = Await.result(resultFuture, 60 seconds)
    println(s"Result arrived: $result")
  }

  def wrapToAsync[A, B](func: A => B): A => Future[B] = { a =>
    Future {
      Thread.sleep(10000)
      func(a)
    }
  }
}
