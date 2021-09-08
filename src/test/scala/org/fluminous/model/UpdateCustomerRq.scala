package org.fluminous.model

case class UpdateCustomerRq(customerId: Integer, name: Option[String], age: Option[Integer])
