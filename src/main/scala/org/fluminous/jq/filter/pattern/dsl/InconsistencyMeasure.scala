package org.fluminous.jq.filter.pattern.dsl

sealed trait InconsistencyMeasure {
  def +(r: InconsistencyMeasure): InconsistencyMeasure
}

case class InconsistencyNumber(number: Int) extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = {
    r match {
      case InconsistencyNumber(ir) => InconsistencyNumber(number + ir)
      case _                       => Infinite
    }
  }
}

case object Infinite extends InconsistencyMeasure {
  override def +(r: InconsistencyMeasure): InconsistencyMeasure = this
}

object InconsistencyMeasure {
  implicit val ordering = new Ordering[InconsistencyMeasure] {
    override def compare(x: InconsistencyMeasure, y: InconsistencyMeasure): Int = {
      (x, y) match {
        case (_: Infinite.type, InconsistencyNumber(_))       => 1
        case (InconsistencyNumber(_), _: Infinite.type)       => -1
        case (_: Infinite.type, _: Infinite.type)             => 0
        case (InconsistencyNumber(l), InconsistencyNumber(r)) => l compare r
      }
    }
  }
}
