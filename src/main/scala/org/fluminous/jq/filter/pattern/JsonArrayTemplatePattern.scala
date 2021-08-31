package org.fluminous.jq.filter.pattern

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.{ JsonArrayTemplate, JsonArrayTemplateConstructor }
import org.fluminous.jq.tokens.RightSquareBracket

class JsonArrayTemplatePattern extends FilterPattern {
  override val FilterCases: PartialFunction[List[Expression], List[Expression]] = {
    case RightSquareBracket :: JsonArrayTemplateConstructor(seq) :: rest =>
      JsonArrayTemplate(seq) :: rest
  }
}
