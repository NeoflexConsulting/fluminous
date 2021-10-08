package org.fluminous.jq.filter.algebra

import org.fluminous.jq.filter.algebra.bool.And
import org.fluminous.jq.filter.algebra.bool.Or

object OperationalIdentifier {
  def apply(identifier: String): Int => AlgebraOperation = {
    operationalIdentifiers(identifier)
  }
  private val operationalIdentifiers = Map[String, Int => AlgebraOperation](
    "or"  -> Or,
    "and" -> And
  )
  val identifiers: Set[String] = operationalIdentifiers.keySet
}
