package org.fluminous.jq.filter.sequence

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{ Description, Expression }

final case class FilterSequenceStart(override val position: Int, filters: List[Filter]) extends Expression {
  override val description: String = FilterSequenceStart.typeDescription.description
}

object FilterSequenceStart {
  implicit def typeDescription: Description[FilterSequenceStart] =
    new Description[FilterSequenceStart] {
      override val description: String = "filters sequence"
    }
}
