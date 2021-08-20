package org.fluminous

import cats.Id
import io.serverlessworkflow.api.workflow.BaseWorkflow
import org.fluminous.routing.{ ExecuteCondition, ExecuteFirstService, ExecuteService, Finish, Routing }
import org.fluminous.services.{ Condition, Service, ServiceCollection }

import scala.io.Source

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
    val isNumber                 = Condition[String]("is_number")(_.forall(_.isDigit))

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
        .addCondition(isNumber)

    val json     = Source.fromResource("routing.json").mkString
    val workflow = BaseWorkflow.fromSource(json)
    val routing  = Routing.fromWorkflow(workflow)
    //Routing information
    /*    val routing = ExecuteFirstService(
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
    )*/
    routing.left.foreach(_.printStackTrace())

    //Creating router
    val router  = serviceCollection.toRouter[String, Customer]
    val result1 = router.routeRequest("Иван", routing.right.get)
    println(result1)
    val result2 = router.routeRequest("12", routing.right.get)
    println(result2)

  }
}
