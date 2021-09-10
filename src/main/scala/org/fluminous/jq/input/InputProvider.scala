package org.fluminous.jq.input

sealed trait InputProvider {
  def position: Int
  def nextSymbol: Symbol
  def moveToNext(): InputProvider
}

private final case class NullProvider(override val position: Int) extends InputProvider {
  override def nextSymbol: Symbol          = EOF
  override def moveToNext(): InputProvider = this
}

private final case class StringProvider(input: String, override val position: Int = 0) extends InputProvider {
  override def nextSymbol: Symbol = input.headOption.fold[Symbol](EOF)(Character)
  override def moveToNext(): InputProvider =
    input.headOption.fold[InputProvider](NullProvider(position))(_ => StringProvider(input.substring(1), position + 1))

}

object InputProvider {
  implicit def strToInputProvider(str: String): InputProvider = InputProvider(str)
  def apply(input: String): InputProvider                     = StringProvider(input)
}
