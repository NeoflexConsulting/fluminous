package org.fluminous.services.rest.model

case class Customer(id: Int, name: String, age: Int)
case class UpdateCustomerRq(customer: Customer)
