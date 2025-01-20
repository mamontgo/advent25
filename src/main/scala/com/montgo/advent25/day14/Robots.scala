package com.montgo.advent25.day14

import com.montgo.advent25.util.{MapUtil, Matrix, Nav, Point}
import Matrix.*
import Nav.*
import Math.floor

case class Quadrants(data: Seq[(Point, Point)] ) {

  def getQuadrant(p: Point): Option[(Point, Point)] = data.find(q =>
    p.row >= q._1.row && p.row <= q._2.row && p.col >= q._1.col && p.col <= q._2.col)

}

case class Dimension(cols: Long, rows: Long) {

  lazy val wrapCol: Long => Long = Robots.teleport(cols)

  lazy val wrapRow: Long => Long = Robots.teleport(rows)

  lazy val matrix: Matrix[Char] =  Matrix((0 to rows.toInt).map(_ => (0 to cols.toInt).map(_ => 'O').toVector).toVector)

  lazy val quadrants: Quadrants =
    val halfRow = floor((rows.toDouble - 1) / 2).toLong
    val halfCol = floor((cols.toDouble - 1) / 2).toLong

    Quadrants(
      Seq(
        (Point(0, 0), Point(halfRow, halfCol)),
        (Point(0, cols - halfCol), Point(halfRow, cols)),
        (Point(rows - halfRow, 0), Point(rows, halfCol)),
        (Point(rows - halfRow, cols - halfCol), Point(rows, cols))
      )
    )


}
case class Room(dimension: Dimension, robots: Seq[Robot]) {
  def move: Room = Room(dimension, robots.map(_.move(dimension)))
  def move(count: Int): Room = (0 until count).foldLeft(this) { (i, _) => i.move }
  def factor: Long = quadrantMap.toSeq.filterNot(_._1 == Robots.UNKNOWN).map(_._2.map(_._2).sum).product
  lazy val matrix: Matrix[Char] = robots.map(_.pos).foldLeft(dimension.matrix) { (i, v) => i.update(v, Robots.ROBOT_CHAR)}
  def prettyPrint(): Unit = matrix.data.printMatrixStyle(p => if(matrix(p) == Robots.ROBOT_CHAR) "31" else "0")
  private def summary: Map[Point, Long] = MapUtil.toMap(robots.map(_.pos), 1L, (_, x) => x + 1)
  private def quadrantMap: Map[(Point, Point), Seq[(Point, Long)]] = MapUtil.mapGroup(summary.toSeq, t => dimension.quadrants.getQuadrant(t._1).getOrElse(Robots.UNKNOWN))

}

case class Robot(pos: Point, velocity: Point) {
  def move(d: Dimension): Robot = Robot(Point(d.wrapRow(pos.row + velocity.row) , d.wrapCol(pos.col + velocity.col) ), velocity)
  def move(d: Dimension, steps: Int): Robot = (0 until steps).foldLeft(this) ((i, _) => i.move(d))
}

object Robots {
  lazy val UNKNOWN: (Point, Point) = (Point(-1, -1), Point(-1, -1))
  val ROBOT_CHAR: Char = 'R'

  def read(in: Seq[String]): Seq[Robot] =
    in.map { r =>
      val parts = r.split(' ')
      val p = parts.head.split('=').last.split(',')
      val v = parts.last.split('=').last.split(',')
      Robot(Point(p.last.toLong, p.head.toLong), Point(v.last.toLong, v.head.toLong))
    }

  def teleport(limit: Long)(x: Long): Long =
    x match
      case v if v < 0 => limit + v + 1
      case v if v > limit => v - limit - 1
      case _ => x

}

object ChristmasTree {

  val SHAPE_ONE: Seq[Point] = Seq(SOUTH, SOUTH_WEST, SOUTH_EAST, SOUTH_WEST + SOUTH_WEST, SOUTH + SOUTH, SOUTH_WEST + SOUTH, SOUTH_EAST + SOUTH )

  def findFirstPatter(r: Room, pattern: Seq[Point], c: Long = 1L): Long =
    val next = r.move
    if (matchPattern(next.matrix, Robots.ROBOT_CHAR, ChristmasTree.SHAPE_ONE)) c else findFirstPatter(next, pattern, c + 1)

  def matchPattern[T](m: Matrix[T], item: T, pattern: Seq[Point]): Boolean =
    m.find(_ == item)
      .map(p => pattern.map(_ + p))
      .filterNot(_.exists(m.outRange))
      .map(_.map(m(_)))
      .exists(l => l.forall(_ == item))

}