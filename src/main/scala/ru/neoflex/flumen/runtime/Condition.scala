package ru.neoflex.flumen.runtime

case class Condition[A](conditionName: String, condition: A => Boolean)
