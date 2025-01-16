package com.montgo.advent25.util

object TupleUtil {
  def map[T, B](t: (T, T), f: T => B): (B, B) =
    (f(t._1), f(t._2))

}
