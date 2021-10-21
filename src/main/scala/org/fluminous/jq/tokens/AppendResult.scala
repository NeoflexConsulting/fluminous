package org.fluminous.jq.tokens

sealed trait AppendResult
final case class TokenUpdated(token: Token) extends AppendResult
final case object TokenConstructed          extends AppendResult
