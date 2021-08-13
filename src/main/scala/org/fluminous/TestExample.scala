package org.fluminous

import cats.Id
import org.fluminous.routing.{ExecuteFirstService, ExecuteService, Finish}
import org.fluminous.services.{Service, ServiceCollection}

case class Customer(name: String, age: Int)

object TestExample {
  def main(args: Array[String]): Unit = {
    //Services
    val upperCaseService         = Service[String, String]("upper", _.toUpperCase)
    val incrementService         = Service[Int, Int]("increment", _ + 1)
    val toStringService          = Service[Int, String]("to_string", _.toString)
    val toIntService             = Service[String, Int]("to_int", _.toInt)
    val getAgeService            = Service[Customer, Int]("get_age", _.age)
    val getNameService           = Service[Customer, String]("get_name", _.name)
    val increaseAgeService       = Service[Customer, Customer]("increase_age", c => c.copy(age = c.age + 1))
    val getCustomerByAgeService  = Service[Int, Customer]("get_customer_by_age", age => Customer("testCustomer", age))
    val getCustomerByNameService = Service[String, Customer]("get_customer_by_name", name => Customer(name, 25))

    //Filling service collection
    val serviceCollection =
      ServiceCollection[Id]()
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

    val result = router.routeRequest("Иван", routing)
    println(result)

  }
}
