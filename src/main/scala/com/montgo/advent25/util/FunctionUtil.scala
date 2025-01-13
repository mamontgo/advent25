package com.montgo.advent25.util

import scala.annotation.tailrec

object FunctionUtil {
  @tailrec def foldUntil[T, K](x: Seq[T], init: K, test: K => Boolean)(f: (K, T) => K): K = {
    x match {
      case h +: tail => val y = f(init, h)
        if (test(y)) y else foldUntil(tail, y, test)(f)
      case _ => init
    }
  }

  @tailrec def repeat[T](i:T, count:Int, resp: Seq[T] = Seq()): Seq[T] =
    if count == 0 then resp else repeat(i, count -1, resp :+ i)



}
