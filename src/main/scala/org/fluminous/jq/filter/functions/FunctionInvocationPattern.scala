package org.fluminous.jq.filter.functions

import org.fluminous.jq.Expression
import org.fluminous.jq.filter.Filter
import org.fluminous.jq.filter.functions.definition.JqFunction
import org.fluminous.jq.filter.pattern.dsl.Matcher.{ capture, captureIf, lookup, test }
import org.fluminous.jq.filter.pattern.{ ExpressionPattern, PatternCases }
import org.fluminous.jq.tokens.Identifier
import org.fluminous.jq.tokens.symbolic.{ Comma, LeftBracket, RightBracket }
import shapeless.{ ::, HNil }

object FunctionInvocationPattern extends ExpressionPattern {
  override protected val ExpressionCases: PatternCases = PatternCases[FunctionNInvocation](
    (lookup[Expression].notInstance[LeftBracket] ->: captureIf[Identifier](id =>
      JqFunction.unaryFunctions.contains(id.value)
    )).ifValidReplaceBy {
      case id :: HNil => UnaryFunctionFunctionInvocation(_, id.value)
    },
    (test[LeftBracket] ->: captureIf[Identifier](id => JqFunction.nNaryFunctions.contains(id.value))).ifValidReplaceBy {
      case id :: HNil => FunctionNInvocationStart(_, id.value, List.empty)
    },
    (test[Comma] ->: capture[Filter] ->: capture[FunctionNInvocationStart]).ifValidReplaceBy {
      case filter :: func :: HNil => FunctionNInvocationStart(_, func.name, filter +: func.parameters)
    },
    (test[RightBracket] ->: capture[Filter] ->: capture[FunctionNInvocationStart]).ifValidReplaceBy {
      case filter :: func :: HNil => FunctionNInvocation(_, func.name, (filter +: func.parameters).reverse)
    }
  )
}
