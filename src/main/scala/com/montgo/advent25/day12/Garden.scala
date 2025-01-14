package com.montgo.advent25.day12

import com.montgo.advent25.util.{Matrix, Nav, Point}

import scala.annotation.tailrec
import Nav.*


object Garden {

  extension (m: Matrix[Char]) {
    def gardenPoints(in: Char): Seq[Point] =
      m.find(_ == in)
  }

  val CORNER_BLOCKS: Seq[Seq[Point]] = Seq(
    Seq(EAST, NORTH_EAST, NORTH),
    Seq(NORTH, NORTH_WEST, WEST),
    Seq(WEST, SOUTH_WEST, SOUTH),
    Seq(SOUTH, SOUTH_EAST, EAST),
  )

  def cornerNeighbours(m: Matrix[Char], isOwnedBlock: Point => Boolean, p: Point): Seq[Seq[Boolean]] =
    CORNER_BLOCKS.map(c => c.map(i => i + p).map(i => if (m.inRange(i)) m(i) == m(p) && isOwnedBlock(i) else false))


  def collectAllGardenStatsPart2(m: Matrix[Char]): Seq[(Long, Long)] =
    gardenTypes(m).map(m.gardenPoints).flatMap(collectGardenTypeStatsPart2(m)(_))

  @tailrec def collectGardenTypeStatsPart2(m: Matrix[Char])(points: Seq[Point], res: Seq[(Long, Long)] = Seq()): Seq[(Long, Long)] =
    val f = findFirstPatch(m, points)
    val stats = res :+ (f._1.size.toLong, f._1.flatMap(cornerNeighbours(m, f._1.contains, _)).count(s => s == Seq(false, false, false) || s == Seq(true, false, true) || s == Seq(false, true, false)).toLong)
    if f._2.isEmpty then
      stats
    else
      collectGardenTypeStatsPart2(m)(f._2, stats)

  def read(in: Seq[String]): Matrix[Char] =
    Matrix(in.map(_.toVector).toVector)

  def gardenTypes(in: Matrix[Char]): Seq[Char] =
    in.find(_=>true).map(in.get).distinct

  @tailrec def findAllPatches(m: Matrix[Char], points: Seq[Point], result: Seq[Seq[Point]]): Seq[Seq[Point]] =
    val t = findFirstPatch(m, points)
    val res = result :+ t._1
    if t._2.isEmpty then
      res
    else
      findAllPatches(m, t._2, res)

  def collectAllGardenStats(m: Matrix[Char]): Seq[(Long, Long)] =
    gardenTypes(m).map(m.gardenPoints).flatMap(collectGardenTypeStats(m)(_))

  @tailrec def collectGardenTypeStats(m: Matrix[Char])(points: Seq[Point], res: Seq[(Long, Long)] = Seq()): Seq[(Long, Long)] =
    val f = findFirstPatch(m, points)
    val stats = res :+ (f._1.size.toLong, calcFences(m, f._1))
    if f._2.isEmpty then
      stats
    else
      collectGardenTypeStats(m)(f._2, stats)

  def findFirstPatch(m: Matrix[Char], points: Seq[Point]): (Seq[Point], Seq[Point]) =
    val p = patch(m, Seq(points.head), Seq(points.head).toSet)
    (p, points.filterNot(p.contains))

  @tailrec def patch(m: Matrix[Char], points: Seq[Point], result: Set[Point] = Set()): Seq[Point] =

    val n = filteredNeighbours(m, points.head, result.toSeq)
    if points.tail.isEmpty && n.isEmpty then
      (result ++ n).toSeq
    else
      patch(m, points.tail ++ n, result ++ n)

  def calcFences(m: Matrix[Char], in: Seq[Point]): Long =
    in.map(p => 4 - filteredNeighbours(m, p, Seq()).count(x => m(x) == m(p))).sum.toLong

  def filteredNeighbours(m: Matrix[Char], point: Point, filtered: Seq[Point] ): Seq[Point] =
    ADJACENT_DIRS
      .map(_ + point)
      .filter(m.inRange)
      .filter(p => m(p) == m(point))
      .filterNot(filtered.contains)
}
