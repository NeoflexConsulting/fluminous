package org.fluminous.jq

sealed trait InputProvider {
  def getPosition(): Int
  def nextSymbol(): Symbol
  def moveToNext(): InputProvider
}

private final case class NullProvider(position: Int) extends InputProvider {
  override def getPosition(): Int          = position
  override def nextSymbol()                = EOF
  override def moveToNext(): InputProvider = this
}

private final case class StringProvider(input: String, position: Int = 0) extends InputProvider {
  override def getPosition(): Int = position
  override def nextSymbol()       = input.headOption.fold[Symbol](EOF)(Character)
  override def moveToNext(): InputProvider =
    input.headOption.fold[InputProvider](NullProvider(position))(_ => StringProvider(input.substring(1), position + 1))

}

object InputProvider {
  def apply(input: String): InputProvider = StringProvider(input)
}
