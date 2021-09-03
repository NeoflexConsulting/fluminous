package org.fluminous

import cats.Id
import io.serverlessworkflow.api.workflow.BaseWorkflow
import io.swagger.parser.OpenAPIParser
import org.fluminous.routing.Routing
import org.fluminous.services.{ Service, ServiceCollection }
import scala.io.Source
import scala.collection.JavaConverters._

case class Customer(name: String, age: Int)

object TestExample {
  def main(args: Array[String]): Unit = {
    import io.circe.generic.auto._
    //Services
    val upperCaseService   = Service[String, String]("upper", _.toUpperCase, "input")
    val incrementService   = Service[Int, Int]("increment", _ + 1, "input")
    val toStringService    = Service[Int, String]("to_string", _.toString, "input")
    val toIntService       = Service[String, Int]("to_int", _.toInt, "input")
    val getAgeService      = Service[Customer, Int]("get_age", _.age, "input")
    val getNameService     = Service[Customer, String]("get_name", _.name, "input")
    val increaseAgeService = Service[Customer, Customer]("increase_age", c => c.copy(age = c.age + 1), "input")
    val getCustomerByAgeService =
      Service[Int, Customer]("get_customer_by_age", Customer("testCustomer", _), "input")
    val getCustomerByNameService =
      Service[String, Customer]("get_customer_by_name", Customer(_, 25), "input")
    val isNumber = Service[String, Boolean]("is_number", _.forall(_.isDigit), "input")

    //Filling service collection
    val serviceCollection =
      ServiceCollection[Id]()
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
    val routing  = Routing.fromWorkflow(workflow)
    val resource = this.getClass.getClassLoader.getResource("CustomerService.json")
    val parser   = new OpenAPIParser().readLocation(resource.toString, null, null)
    parser.getMessages.asScala.foreach(println)
    val openApi = parser.getOpenAPI
    openApi.getServers.asScala.foreach(s => println(s.getUrl))
    //Creating router
    routing.foreach { r =>
      val router  = serviceCollection.toRouter[String, Customer](r)
      val result1 = router.routeRequest("Иван")
      println(result1)
      val result2 = router.routeRequest("12")
      println(result2)
    }

  }
}
