package ru.neoflex.flumen

case class Condition[A] (conditionName: String, condition:A=>Boolean)
