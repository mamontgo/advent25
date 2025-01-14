package com.montgo.advent25.day10

import com.montgo.advent25.util.{Matrix, Point}
import com.montgo.advent25.util.Nav.*

import scala.annotation.tailrec
object TrailHead {


  val END = 9

  def read(in: Seq[String]): Matrix[Int] =
    Matrix(in.map(_.map(_.asDigit).toVector).toVector)

  def getTrailHeads(in: Matrix[Int]): Seq[Point] =
    in.find(_==0)

  @tailrec def nav(m: Matrix[Int])(p:Seq[Point], res: Seq[Point] = Seq()): Seq[Point] =
    if p.isEmpty then
      res
    else
      val x = m.get(p.head)
      if (x == END) nav(m)(p.tail, res :+ p.head)
      else nav(m)(p.tail ++ ADJACENT_DIRS.map(_+p.head).filter(m.inRange).filter(p => m.get(p)==x+1), res)



}
