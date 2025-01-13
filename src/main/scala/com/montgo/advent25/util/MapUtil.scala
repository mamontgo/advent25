package com.montgo.advent25.util

object MapUtil {
  def toMap[T, Y](in: Seq[T], init: Y, f: (T, Y) => Y): Map[T, Y] =
    in.foldLeft(Map[T, Y]()) { (i, v) =>
      i + i.get(v).map(x => v -> f(v, x)).getOrElse(v -> init)
    }

  def mapMerge[K, T](first: Map[K, T], second: Map[K, T], f: (T, T) => T): Map[K, T] =
    (first.keys ++ second.keys).toSet.foldLeft(Map[K,T]()) { (i, v) =>
      first.get(v).map(t1 => second.get(v).map(t2 => f(t1,t2)).getOrElse(t1)).orElse(second.get(v))
        .map(x => i + (v -> x)).getOrElse(i)
    }

}
