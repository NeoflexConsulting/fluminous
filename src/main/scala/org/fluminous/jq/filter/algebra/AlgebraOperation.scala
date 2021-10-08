package org.fluminous.jq.filter.algebra

import io.circe.Json.Null
import io.circe.{ Json, JsonNumber, JsonObject }
import org.fluminous.jq.{ EvaluationException, Expression }
import io.circe.syntax._
import cats.syntax.traverse._

import java.util.regex.Pattern

sealed trait OperationSign extends Expression {
  val priority: Int
  def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json]
  protected def asBoolean(json: Json): Boolean = {
    json.asBoolean.getOrElse {
      json.asNull.fold(true)(_ => false)
    }
  }
  protected def asNumber(json: Json): Option[JsonNumber] = {
    json.asNull.fold(json)(_ => json).asNumber
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
  val Zero: Json         = Json.fromInt(0)
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
    addNumbers(left, right)
      .orElse(addArrays(left, right))
      .orElse(addStrings(left, right))
      .orElse(addObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }

  private def addNumbers(left: Json, right: Json): Option[Json] = {
    for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
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
    subtractNumbers(left, right)
      .orElse(subtractArrays(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
  private def subtractNumbers(left: Json, right: Json): Option[Json] = {
    for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal - rightBigDecimal)
  }
  private def subtractArrays(left: Json, right: Json): Option[Json] = {
    for {
      leftArray  <- left.asArray
      rightArray <- right.asArray
    } yield Json.fromValues(leftArray.filterNot(rightArray.toSet.contains))
  }

}
final case class MultiplicationSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "multiplication"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(multiply(left, _))
  }
  private def multiply(left: Json, right: Json): Either[EvaluationException, Json] = {
    multiplyNumbers(left, right)
      .orElse(multiplyStringByNumber(left, right))
      .orElse(multiplyObjects(left, right))
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }

  private def multiplyStringByNumber(left: Json, right: Json): Option[Json] = {
    for {
      rightNumber <- asNumber(right)
      rightInt    <- rightNumber.toInt
      leftString  <- left.asString
    } yield if (rightInt == 0) Null else Json.fromString(Array.fill(rightInt)(leftString).mkString)
  }

  private def multiplyNumbers(left: Json, right: Json): Option[Json] = {
    for {
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
    } yield Json.fromBigDecimal(leftBigDecimal * rightBigDecimal)
  }
  private def multiplyObjects(left: Json, right: Json): Option[Json] = {
    for {
      rightObject <- right.asObject
      leftObject  <- left.asObject
    } yield Json.fromJsonObject(leftObject.deepMerge(rightObject))
  }
}
final case class DivisionSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "division"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(divide(left, _))
  }
  private def divide(left: Json, right: Json): Either[EvaluationException, Json] = {
    divideNumbers(left, right).flatMap {
      _.orElse(divideStrings(left, right))
        .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
    }
  }
  private def divideNumbers(left: Json, right: Json): Either[EvaluationException, Option[Json]] = {
    (for {
      leftNumber      <- asNumber(left)
      leftBigDecimal  <- leftNumber.toBigDecimal
      rightNumber     <- asNumber(right)
      rightBigDecimal <- rightNumber.toBigDecimal
    } yield {
      if (rightBigDecimal == 0)
        Left(EvaluationException(position, "Division by zero"))
      else
        Right(Json.fromBigDecimal(leftBigDecimal / rightBigDecimal))
    }).sequence
  }

  private def divideStrings(left: Json, right: Json): Option[Json] = {
    for {
      leftString  <- left.asString
      rightString <- right.asString
    } yield Json.fromValues(leftString.split(Pattern.quote(rightString)).map(Json.fromString))
  }

}
final case class ModuloSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 4
  override val description: String = "modulo"

  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(moduloNumbers(left, _))
  }

  private def moduloNumbers(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber  <- asNumber(left)
      leftInt     <- leftNumber.toInt
      rightNumber <- asNumber(right)
      rightInt    <- rightNumber.toInt
    } yield {
      if (rightInt == 0)
        Left(EvaluationException(position, "Modulo by zero"))
      else
        Right(Json.fromBigDecimal(leftInt % rightInt))
    }).sequence.flatMap(
      _.toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
    )
  }
}

final case class EqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "equal"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.map(isEqual(left, _))
  }
  private def isEqual(left: Json, right: Json): Json = {
    (left == right).asJson
  }
}

final case class NotEqual(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "not equal"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.map(notEqual(left, _))
  }
  private def notEqual(left: Json, right: Json): Json = {
    (left != right).asJson
  }
}

final case class GreaterOrEqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "greater or equal"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(greaterOrEqual(left, _))
  }
  private def greaterOrEqual(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal >= rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}

final case class LessOrEqualSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "less or equal"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(lessOrEqual(left, _))
  }
  private def lessOrEqual(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal <= rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}

final case class GreaterSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "greater"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(greater(left, _))
  }
  private def greater(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal > rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}

final case class LessSign(override val position: Int) extends OperationSign {
  override val priority: Int       = 0
  override val description: String = "less"
  override def execute(left: Json, right: => Either[EvaluationException, Json]): Either[EvaluationException, Json] = {
    right.flatMap(less(left, _))
  }
  private def less(left: Json, right: Json): Either[EvaluationException, Json] = {
    (for {
      leftNumber   <- left.asNumber
      leftDecimal  <- leftNumber.toBigDecimal
      rightNumber  <- right.asNumber
      rightDecimal <- rightNumber.toBigDecimal
    } yield (leftDecimal < rightDecimal).asJson)
      .toRight(EvaluationException(position, s"${left.name} and ${right.name}  are incompatible for $description"))
  }
}
