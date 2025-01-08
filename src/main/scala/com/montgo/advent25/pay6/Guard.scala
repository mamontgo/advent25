package com.montgo.advent25.pay6

import com.montgo.advent25.pay6.Dir.{DOWN, LEFT, RIGHT, UP}
import com.montgo.advent25.pay6.Position.{Direction, Obstacle}
import com.montgo.advent25.util.{Matrix, Point}

import scala.annotation.tailrec

enum Dir {
  case UP
  case DOWN
  case LEFT
  case RIGHT
}

enum Position {
  case Obstacle
  case Space
  case Direction(d:Dir)
}

case class Navigate(point: Point, dir: Dir) {
  def up: Navigate = Navigate(point.up, dir)
  def down: Navigate = Navigate(point.down, dir)
  def left: Navigate = Navigate(point.left, dir)
  def right: Navigate = Navigate(point.right, dir)
}

object Guard {
  private val UP_CHAR = '^'
  private val DOWN_CHAR = 'v'
  private val LEFT_CHAR = '<'
  private val RIGHT_CHAR = '>'
  private val SPACE_CHAR = '.'
  private val OBSTACLE_CHAR = '#'

  def isDir(in: Char): Boolean = in == UP_CHAR || in == DOWN_CHAR || in == LEFT_CHAR || in == RIGHT_CHAR

  private def asDir(in: Char): Dir =
    in match
      case UP_CHAR => Dir.UP
      case DOWN_CHAR => Dir.DOWN
      case LEFT_CHAR => Dir.LEFT
      case _ => Dir.RIGHT

  private def asPosition(in: Char): Position =
    in match
      case SPACE_CHAR => Position.Space
      case OBSTACLE_CHAR => Position.Obstacle
      case x => Direction(asDir(x))

  def asMatrix(in: Seq[String]): Matrix[Position] =
    Matrix(in.map(s => s.map(asPosition).toVector).toVector)

  def findSteps(m: Matrix[Position]): Seq[Navigate] =
    doWalk(m, getStart(m))._1

  def findObstaclePositions(m: Matrix[Position]): Seq[Point] =
    val start = getStart(m)
    (0 until m.width).foldLeft(Seq[Point]()) { (init, col) =>
      println(s"Processing col $col.  Found ${init.length}."  )

      init ++ (0 until m.height).foldLeft(Seq[Point]()) { (i, row)  =>
        val p = Point(row, col)
        m.get(p) match
          case Position.Space =>
            if doWalk(m.update(p, Obstacle), start)._2 then i :+ p else i
          case _ => i
      }
    }

  def getStart(m: Matrix[Position]): Navigate =
    val start = m.find { case Direction(_) => true case _ => false }.head
    val dir = m.get(start).asInstanceOf[Direction].d
    Navigate(start, dir)

  def doWalk(matrix: Matrix[Position], n: Navigate): (Seq[Navigate], Boolean) =
    walk(matrix, n, Seq(n))


  @tailrec private def walk(matrix: Matrix[Position], n: Navigate, path: Seq[Navigate]): (Seq[Navigate], Boolean) =
    val next = navigate(n)
    if !matrix.inRange(next.point) then
      (path, false)
    else if path.contains(next) then
      (path, true)
    else if matrix.get(next.point) == Obstacle then
      walk(matrix, turn(n), path)
    else
      walk(matrix, next, path :+ next)

  private def navigate(in: Navigate):Navigate =
    in.dir match
      case UP => in.up
      case DOWN => in.down
      case LEFT => in.left
      case RIGHT => in.right

  private def turn(in: Navigate): Navigate =
    in.dir match
      case UP => Navigate(in.point, RIGHT)
      case DOWN => Navigate(in.point, LEFT)
      case LEFT => Navigate(in.point, UP)
      case RIGHT => Navigate(in.point, DOWN)
}
