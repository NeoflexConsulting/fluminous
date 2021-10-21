package org.fluminous.runtime

trait ExecutorFunctions {
  protected def getUniqueValue[K, V, E](ex: => E)(pair: (K, List[V])): Either[E, (K, V)] = {
    pair match {
      case (name, values) =>
        getUnique(ex,values).map(j => (name, j))
    }
  }

  protected def getUnique[V, E](ex: => E, list: List[V]): Either[E, V] = {
    list match {
      case head :: _ => Right(head)
      case _         => Left(ex)
    }
  }
}
