package org.fluminous.jq.filter.selector

import io.circe.Json
import org.fluminous.jq.EvaluationException
import org.fluminous.jq.filter.Filter

final case class IdentitySelector(override val position: Int) extends Filter {
  override val isSingleValued: Boolean = true
  override def transform(input: Json): Either[EvaluationException, List[Json]] = {
    Right(List(input))
  }
  override val description: String = s"root selector"
}
