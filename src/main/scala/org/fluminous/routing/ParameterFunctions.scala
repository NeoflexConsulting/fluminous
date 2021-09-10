package org.fluminous.routing

import cats.data.Validated
import cats.data.Validated.{ Invalid, Valid }
import io.circe.Json
import io.swagger.v3.oas.models.parameters.Parameter
import org.fluminous.services.{
  JSONInputParameterType,
  RequiredInputParameterIsMissing,
  ValidationServiceError,
  ValidationServiceFailure
}
import cats.syntax.traverse._

trait ParameterFunctions {

  def isRequired(p: Parameter): Boolean = {
    val b: Boolean = p.getRequired
    Option(b).getOrElse(false)
  }
  def isOptional(p: Parameter): Boolean = !isRequired(p)
  def isPath(p: Parameter): Boolean     = p.getIn == "path"
  def isHeader(p: Parameter): Boolean   = p.getIn == "header"
  def isQuery(p: Parameter): Boolean    = p.getIn == "query"

  def validateRequiredParameters(
    operationId: String,
    expected: List[(String, Boolean)],
    actual: Map[String, Json]
  ): Either[ValidationServiceError, List[(String, String)]] = {
    expected.traverse(validateParameter(actual)).leftMap(ValidationServiceError(operationId, _)).map(_.flatten).toEither
  }

  private def validateParameter(
    inputParameters: Map[String, Json]
  )(
    validatedParameter: (String, Boolean)
  ): Validated[List[ValidationServiceFailure], Option[(String, String)]] = {
    val name           = validatedParameter._1
    val isRequired     = validatedParameter._2
    val inputParameter = inputParameters.get(name)
    if (inputParameter.isEmpty && isRequired)
      Invalid(List(RequiredInputParameterIsMissing(name)))
    else if (inputParameter.exists(nonPrimitive))
      Invalid(
        List(
          JSONInputParameterType(
            name,
            inputParameter.exists(_.isArray),
            inputParameter.exists(_.isObject),
            inputParameter.exists(_.isNull)
          )
        )
      )
    else Valid(inputParameter.map(p => (name, p.toString)))
  }

  private def nonPrimitive(j: Json): Boolean = j.isArray || j.isObject || j.isNull
}
