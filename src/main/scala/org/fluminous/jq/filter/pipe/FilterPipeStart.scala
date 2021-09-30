package org.fluminous.jq.filter.pipe

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class FilterPipeStart(override val position: Int, filters: Seq[Filter]) extends Expression {
  override val description: String = FilterPipeStart.typeDescription.description
}

object FilterPipeStart {
  implicit def typeDescription: Description[FilterPipeStart] =
    new Description[FilterPipeStart] {
      override val description: String = "filters pipe"
    }
}
