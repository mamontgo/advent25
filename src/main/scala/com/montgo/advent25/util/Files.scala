package com.montgo.advent25.util

import scala.io.Source

object Files {
  def lines(in: String): Seq[String] =
    val source = Source.fromFile(in)
    source.getLines().toSeq
    
}
