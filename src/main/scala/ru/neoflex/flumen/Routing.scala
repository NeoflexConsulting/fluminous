package ru.neoflex.flumen

trait Routing {
  def nextStep(lastStep:String): String
}
