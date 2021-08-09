package ru.neoflex.flumen

import ru.neoflex.flumen.matrix.{ Service, ServiceMatrix, ServicesWithInput, ServicesWithOutput }

class NotFoundException extends Exception

// TODO Implement Unique typeclasses

object Test {
  def main(args: Array[String]): Unit = {
    val upperCaseService = Service[String, String]("upper", _.toUpperCase)
    val incrementService = Service[Int, Int]("increment", _ + 1)
    val toStringService  = Service[Int, String]("to_string", _.toString)
    val toIntService     = Service[String, Int]("to_int", _.toInt)

    val serviceMatrix = ServiceMatrix(upperCaseService)
    val bigServiceMatrix =
      serviceMatrix.enlarge(ServicesWithInput(toStringService), ServicesWithOutput(toIntService), incrementService)

    println(serviceMatrix.get[String, String].invoke("some String from Service Matrix1"))
    println(bigServiceMatrix.get[Int, Int].invoke(3))
    println(bigServiceMatrix.get[Int, String].invoke(3))
    println(bigServiceMatrix.get[String, Int].invoke("3"))
    println(bigServiceMatrix.get[String, String].invoke("some String"))
  }
}
