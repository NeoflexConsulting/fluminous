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
