package org.fluminous.jq

sealed trait Symbol

final case object EOF extends Symbol

final case class Character(c: Char) extends Symbol
