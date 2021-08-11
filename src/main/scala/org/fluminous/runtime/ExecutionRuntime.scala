package org.fluminous.runtime

trait ExecutionRuntime {
  def invokeService(serviceName: String, outputVariable: String): Unit = { ??? }
  def checkCondition(conditionName: String): Boolean                   = { ??? }
  protected val runtime: Seq[TypeInfo]

  override def toString: String = runtime.mkString("\n")
}
