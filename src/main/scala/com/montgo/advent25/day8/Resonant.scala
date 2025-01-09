package com.montgo.advent25.day8

import com.montgo.advent25.util.{Matrix, Point}

import scala.annotation.tailrec

case class Antenna(value: Option[Char], antinode: Boolean = false) {
  def anti: Antenna = Antenna(value, true)
}


object Resonant {

  def read(in:Seq[String]): Matrix[Antenna] =
    Matrix(in.map(_.map(c => if c == '.' then Antenna(None) else Antenna(Some(c))).toVector).toVector)

  def antennaMap(in: Matrix[Antenna]): Map[Char, Seq[Point]] =
    in.find(_.value.isDefined).foldLeft(Map[Char, Seq[Point]]()) { (i, v) =>
      val key = in.get(v).value.get
      i + i.get(key).map(s => key -> (s :+ v)).getOrElse(key -> Seq(v))
    }

  def setAllAntinodePoints(matrix: Matrix[Antenna]): Matrix[Antenna] =
    antennaMap(matrix).values.flatMap(allAntinodePoints(matrix)).filter(matrix.inRange).foldLeft(matrix) { (o, v) =>
      o.update(v, o.get(v).anti)
    }

  def setAntinodePoints(matrix: Matrix[Antenna]): Matrix[Antenna] =
    antennaMap(matrix).values.flatMap(antinodePoints).filter(matrix.inRange).foldLeft(matrix) { (o, v) =>
      o.update(v, o.get(v).anti)
    }

  def allAntinodePoints(matrix: Matrix[Antenna])(in: Seq[Point]): Seq[Point] =
    if in.isEmpty || in.size < 2 then
      Seq()
    else
      getAntiPoints(matrix)(in.head, in.tail) ++ in


  def antinodePoints(in:Seq[Point]): Seq[Point] =
    if in.isEmpty || in.size < 2 then
      Seq()
    else
      getPoints(in.head, in.tail)

  @tailrec def getPoints(current: Point, remaining: Seq[Point], result: Seq[Point] = Seq()): Seq[Point] =
    if remaining.isEmpty then
      result
    else
      val all = remaining.flatMap(r => Seq(r + (r - current), current + (current - r)))
      getPoints(remaining.head, remaining.tail, result ++ all)

  @tailrec def getAntiPoints(in: Matrix[Antenna])(current: Point, remaining: Seq[Point], result: Seq[Point] = Seq()): Seq[Point] =
    if remaining.isEmpty then
      result
    else
      val all = remaining.flatMap(r => getAllPoints(in)(r, current) ++ getAllPoints(in)(current, r))
      getAntiPoints(in)(remaining.head, remaining.tail, result ++ all)

  @tailrec def getAllPoints(in: Matrix[Antenna])(a: Point, b: Point, res: Seq[Point] = Seq()): Seq[Point] =
    val next = a + (a -b)
    if !in.inRange(next) then
      res
    else
      getAllPoints(in)(next, a, res :+ next)

}
