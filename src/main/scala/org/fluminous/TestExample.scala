package org.fluminous

import org.fluminous.matrix.{ Service, ServiceMatrix, ServicesWithInput, ServicesWithOutput }

class NotFoundException extends Exception

// TODO Implement Unique typeclasses

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

    val fromIntServices      = ServicesWithInput(toStringService)
    val toIntServices        = ServicesWithOutput(toIntService)
    val fromCustomerServices = ServicesWithInput(getNameService).append(Seq(getAgeService))
    val toCustomerServices   = ServicesWithOutput(getCustomerByNameService).append(Seq(getCustomerByAgeService))

    val serviceMatrix = ServiceMatrix(upperCaseService)

    val bigServiceMatrix =
      serviceMatrix
        .appendType(fromIntServices, toIntServices, Seq(incrementService))
        .appendType(fromCustomerServices, toCustomerServices, Seq(increaseAgeService))

    for {
      executionRuntime <- bigServiceMatrix.toExecutionRuntime
    } {
      println(executionRuntime)
      executionRuntime.invokeService("upper", "upper_cased")
    }
  }
}
