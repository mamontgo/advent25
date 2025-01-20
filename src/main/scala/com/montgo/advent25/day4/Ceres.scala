package com.montgo.advent25.day4

import com.montgo.advent25.util.{Matrix, Point}
import scala.annotation.tailrec


case class LineCollector[T](matrix: Matrix[T], point: Point, nextRow: Int => Int, nextCol: Int => Int) {
  private def nextPoint(): Point = Point(nextRow(point.row.toInt), nextCol(point.col.toInt))
  def getNext: Option[LineCollector[T]] =
    val next = nextPoint()
    if matrix.inRange(next) then
      Some(this.copy(point = next))
    else
      None
  def value: T = matrix.get(point)
}



object Ceres {

  private val M = 'M'
  private val S = 'S'
  private val A = 'A'

  def countXmasDiagonals(in: Matrix[Char]): Int =
    (1 to in.width - 2).flatMap(c => (1 to in.height - 2).map(r =>  in.get(r, c) == A && isXmasDiagonals(Point(r, c), in))).count(_ == true)

  private def isXmasDiagonals(point: Point, in: Matrix[Char]): Boolean =
    (in.get(point.row - 1, point.col - 1) == M &&  in.get(point.row + 1, point.col + 1) == S || in.get(point.row - 1, point.col - 1) == S &&  in.get(point.row + 1, point.col + 1) == M) &&
    (in.get(point.row - 1, point.col + 1) == M && in.get(point.row + 1, point.col - 1) == S || in.get(point.row - 1, point.col + 1) == S && in.get(point.row + 1, point.col - 1) == M)


  private val INC: Int => Int = x => x + 1
  private val DE_INC: Int => Int = x => x - 1

  def countOccurrences(tgt: String, in: Matrix[Char]): Int =
    val rev = tgt.reverse
    getLines(in).map(src => countLineOccurrences(src, tgt) + countLineOccurrences(src, rev)).sum


  private def countLineOccurrences(src: String, tgt: String): Int =
    src.sliding(tgt.length).count(window => window == tgt)

  def read(in: Seq[String]): Matrix[Char] =
    Matrix(in.map(s => s.toCharArray.toVector).toVector)

  def getLines(in: Matrix[Char]): Seq[String] =
    (0 until in.width).map(c => collector(LineCollector(in, Point(0, c), INC, identity))) ++
    (0 until in.height).map(r => collector(LineCollector(in, Point(r, 0), identity, INC))) ++
    (0 until in.width).map(c => collector(LineCollector(in, Point(0, c), INC, DE_INC))) ++
    (1 until in.height).map(r => collector(LineCollector(in, Point(r, in.width - 1), INC, DE_INC))) ++
    (0 until in.width).map(c => collector(LineCollector(in, Point(0, c), INC, INC))) ++
    (1 until in.height).map(r => collector(LineCollector(in, Point(r, 0), INC, INC)))

  private def collector(in: LineCollector[Char]) = lineCollector(Some(in))

  @tailrec private def lineCollector(collector: Option[LineCollector[Char]], res: String = ""): String =
    collector match
      case Some(l) => lineCollector(l.getNext, res :+ l.value)
      case _ => res
}
