package org.fluminous.jq.filter.pattern.dsl

import cats.Order

sealed trait MismatchesQty {
  def +(r: MismatchesQty): MismatchesQty
}

case class MismatchNumber(number: Int) extends MismatchesQty {
  override def +(r: MismatchesQty): MismatchesQty = {
    r match {
      case MismatchNumber(ir) => MismatchNumber(number + ir)
      case _                  => CompleteMismatch
    }
  }
}

case object CompleteMismatch extends MismatchesQty {
  override def +(r: MismatchesQty): MismatchesQty = this
}

object MismatchesQty {
  implicit val ordering = new Ordering[MismatchesQty] {
    override def compare(x: MismatchesQty, y: MismatchesQty): Int = {
      (x, y) match {
        case (_: CompleteMismatch.type, MismatchNumber(_))        => 1
        case (MismatchNumber(_), _: CompleteMismatch.type)        => -1
        case (_: CompleteMismatch.type, _: CompleteMismatch.type) => 0
        case (MismatchNumber(l), MismatchNumber(r))               => l compare r
      }
    }
  }

  implicit val order = new Order[MismatchesQty] {
    override def compare(x: MismatchesQty, y: MismatchesQty): Int = {
      (x, y) match {
        case (_: CompleteMismatch.type, MismatchNumber(_))        => 1
        case (MismatchNumber(_), _: CompleteMismatch.type)        => -1
        case (_: CompleteMismatch.type, _: CompleteMismatch.type) => 0
        case (MismatchNumber(l), MismatchNumber(r))               => l compare r
      }
    }
  }
}
