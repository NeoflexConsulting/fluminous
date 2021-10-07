package org.fluminous.jq.filter.algebra

import io.circe.{ Json, JsonObject }
import org.fluminous.jq.{ EvaluationException, Expression }
import io.circe.syntax._

sealed trait OperationSign extends Expression {
  val priority: Int
  def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json]
  protected def asBoolean(json: Json): Boolean = {
    json.asBoolean.getOrElse {
      json.asNull.fold(true)(_ => false)
    }
  }
}

object OperationSign {
  def apply(sign: String): Int => OperationSign = {
    operations(sign)
  }
  private val operations = Map[String, Int => OperationSign](
    "or"  -> OrSign,
    "and" -> AndSign,
    "+"   -> AdditionSign,
    "-"   -> SubtractionSign,
    "*"   -> MultiplicationSign,
    "/"   -> DivisionSign,
    "%"   -> ModuloSign,
    "=="  -> EqualSign,
    "!="  -> NotEqual,
    ">="  -> GreaterOrEqualSign,
    "<="  -> LessOrEqualSign,
    ">"   -> GreaterSign,
    "<"   -> LessSign
  )
  val signs: Set[String] = operations.keySet
}

final case class OrSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 1
  override val description: String = "or"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    val leftBoolean = asBoolean(left)
    if (leftBoolean) {
      Right(Json.True)
    } else {
      for {
        evaluated <- right
      } yield {
        (leftBoolean || asBoolean(evaluated)).asJson
      }
    }
  }
}
final case class AndSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 2
  override val description: String = "and"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    val leftBoolean = asBoolean(left)
    if (!leftBoolean) {
      Right(Json.False)
    } else {
      for {
        evaluated <- right
      } yield {
        (leftBoolean && asBoolean(evaluated)).asJson
      }
    }
  }
}
final case class AdditionSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 3
  override val description: String = "addition"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(add(left, _))
  }

  private def add(left: Json, right: Json): Either[EvaluationException, Json] = {
    left.asNull
      .map(_ => right)
      .orElse(right.asNull.map(_ => left))
      .orElse(addNumbers(left, right))
      .orElse(addArrays(left, right))
      .orElse(addStrings(left, right))
      .orElse(addObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for addition"))
  }

  private def addNumbers(left: Json, right: Json): Option[Json] = {
    for {
      leftNumber      <- left.asNumber
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- right.asNumber
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal + rightBigDecimal)
  }

  private def addArrays(left: Json, right: Json): Option[Json] = {
    for {
      leftArray  <- left.asArray
      rightArray <- right.asArray
    } yield Json.fromValues(leftArray ++ rightArray)
  }
  private def addStrings(left: Json, right: Json): Option[Json] = {
    for {
      leftString  <- left.asString
      rightString <- right.asString
    } yield Json.fromString(leftString ++ rightString)
  }
  private def addObjects(left: Json, right: Json): Option[Json] = {
    for {
      leftObject  <- left.asObject
      rightObject <- right.asObject
    } yield Json.fromJsonObject(mergeObjects(rightObject, leftObject))
  }

  private def mergeObjects(fromObject: JsonObject, toObject: JsonObject): JsonObject =
    fromObject.toIterable.foldLeft(toObject) {
      case (to, (fromKey, fromValue)) =>
        toObject(fromKey).fold(to.add(fromKey, fromValue)) { _ =>
          to.add(fromKey, fromValue)
        }
    }
}

final case class SubtractionSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 3
  override val description: String = "subtraction"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(subtract(left, _))
  }

  private def subtract(left: Json, right: Json): Either[EvaluationException, Json] = {
    left.asNull
      .map(_ => right)
      .orElse(right.asNull.map(_ => left))
      .orElse(addNumbers(left, right))
      .orElse(addArrays(left, right))
      .orElse(addStrings(left, right))
      .orElse(addObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for addition"))
  }
}
final case class MultiplicationSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "multiplication"
}
final case class DivisionSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "division"
}
final case class ModuloSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "modulo"
}

final case class EqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "equal"
}

final case class NotEqual(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "not equal"
}

final case class GreaterOrEqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "greater or equal"
}

final case class LessOrEqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "less or equal"
}

final case class GreaterSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "greater"
}

final case class LessSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "less"
}
