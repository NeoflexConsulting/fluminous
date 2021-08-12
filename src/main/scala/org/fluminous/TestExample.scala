package org.fluminous

import org.fluminous.matrix.{ServiceCollection, Service}

case class Customer(name: String, age: Int)

object TestExample {
  def main(args: Array[String]): Unit = {

    val upperCaseService         = Service[String, String]("upper", _.toUpperCase)
    val incrementService         = Service[Int, Int]("increment", _ + 1)
    val toStringService          = Service[Int, String]("to_string", _.toString)
    val toIntService             = Service[String, Int]("to_int", _.toInt)
    val getAgeService            = Service[Customer, Int]("get_age", _.age)
    val getNameService           = Service[Customer, String]("get_name", _.name)
    val increaseAgeService       = Service[Customer, Customer]("increase_age", c => c.copy(age = c.age + 1))
    val getCustomerByAgeService  = Service[Int, Customer]("get_customer_by_age", age => Customer("testCustomer", age))
    val getCustomerByNameService = Service[String, Customer]("get_customer_by_name", name => Customer(name, 25))
    val serviceMatrix =
      ServiceCollection()
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
    for {
      executionRuntime <- serviceMatrix.toExecutionRuntime
    } {
      println(executionRuntime)
      executionRuntime.invokeService("upper", "upper_cased")
    }
  }
}
