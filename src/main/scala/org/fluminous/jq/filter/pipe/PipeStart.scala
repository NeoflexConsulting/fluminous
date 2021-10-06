package org.fluminous.jq.filter.pipe

import org.fluminous.jq.filter.Filter
import org.fluminous.jq.{Description, Expression}

final case class PipeStart(override val position: Int, filters: List[Filter]) extends Expression {
  override val description: String = PipeStart.typeDescription.description
}

object PipeStart {
  implicit def typeDescription: Description[PipeStart] =
    new Description[PipeStart] {
      override val description: String = "filters pipe"
    }
}
