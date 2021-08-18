package org.fluminous

import org.fluminous.routing.{ ExecuteCondition, ExecuteFirstService, ExecuteService, Finish }
import org.fluminous.services.{ AsyncService, Condition, ServiceCollection }

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
    val isNumber = Condition[String]("is_number")(_.forall(_.isDigit))

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
        .addCondition(isNumber)

    //Routing information
    val routing = ExecuteFirstService(
      "upper",
      "upperInput",
      ExecuteCondition(
        "is_number",
        "upperInput",
        ExecuteService(
          "to_int",
          "upperInput",
          "age",
          ExecuteService("get_customer_by_age", "age", "result", Finish("result"))
        ),
        ExecuteService("get_customer_by_name", "upperInput", "result", Finish("result"))
      )
    )
    //Creating router
    val router = serviceCollection.toRouter[String, Customer]

    val resultFuture1 = router.routeRequest("Иван", routing)
    println("Awaiting result1.....")
    val result1 = Await.result(resultFuture1, 60 seconds)
    println(s"Result1 arrived: $result1")

    val resultFuture2 = router.routeRequest("12", routing)
    println("Awaiting result2.....")
    val result2 = Await.result(resultFuture2, 60 seconds)
    println(s"Result2 arrived: $result2")
  }

  def wrapToAsync[A, B](func: A => B): A => Future[B] = { a =>
    Future {
      Thread.sleep(10000)
      func(a)
    }
  }
}
