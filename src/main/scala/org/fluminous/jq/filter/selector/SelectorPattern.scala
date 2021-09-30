package org.fluminous.jq.filter.selector

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.filter.pipe.FilterPipeStart
import org.fluminous.jq.tokens.{ Identifier, RawString, Root }
import shapeless.{ ::, HNil }

/*case object SelectorPattern extends ExpressionPattern {

}*/
