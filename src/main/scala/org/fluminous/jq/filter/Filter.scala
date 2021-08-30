package org.fluminous.jq.filter

import org.fluminous.jq.Expression

sealed trait Filter extends Expression

final case class Selector(path: Seq[String]) extends Filter
