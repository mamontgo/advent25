package com.montgo.advent25.day15

import com.montgo.advent25.util.{Matrix, Nav, Point}
import com.montgo.advent25.util.Matrix.*

import scala.annotation.tailrec

enum Dir {
  case Up
  case Down
  case Left
  case Right
}

object Dir {
  val UpChar: Char = '^'
  val DownChar: Char = 'v'
  val LeftChar: Char = '<'
  val RightChar: Char = '>'

  def fromChar(in: Char): Dir =
    in match
      case UpChar => Up
      case DownChar => Down
      case LeftChar => Left
      case _ => Right
}

case class WarehouseState(m: Matrix[Char], loc: Point, instructions: Seq[Dir]) {

  def get(p: Point): Char = m(p)
  def removeInstruction(): WarehouseState = this.copy(instructions = instructions.tail)
  def expand: WarehouseState =
    val e = Warehouse.expandBox(m)
    WarehouseState(e, e.find(_ == Warehouse.Robot).head, instructions)

  private def itemScore(c: Char): Long =
    (0 until m.width).foldLeft(0) { (tally, col) =>
      (0 until m.height).foldLeft(tally) { (t, row) => if (m(Point(row, col)) == c) t + (row * 100) + col else t}
    }

  def boxScore: Long = itemScore(Warehouse.BoxLeft)

  def score: Long = itemScore(Warehouse.Box)
}

object Warehouse {
  private val Directions: Map[Dir, Point] = Map[Dir, Point](Dir.Up -> Nav.NORTH, Dir.Down -> Nav.SOUTH, Dir.Left -> Nav.WEST, Dir.Right -> Nav.EAST)
  val Robot: Char = '@'
  val Wall: Char = '#'
  val Box: Char = 'O'
  val Space: Char = '.'
  val BoxLeft = '['
  val BoxRight = ']'

  def read(in: Seq[String]): WarehouseState =
    val parts = in.splitAt(in.indexOf(""))
    val m = Matrix(parts._1.map(_.toVector).toVector)
    WarehouseState(m, m.find(_ == Robot).head, parts._2.flatMap(_.map(Dir.fromChar)))


  def expandBox(m: Matrix[Char]): Matrix[Char] =
    Matrix(m.data.map { row =>
      row.foldLeft(Vector[Char]()) { (i, v) =>
        v match
          case Wall => i :+ Wall :+ Wall
          case Box => i :+ BoxLeft :+ BoxRight
          case Robot => i :+ Robot :+ Space
          case _ => i :+ Space :+ Space
      }
    })


  @tailrec def moveAll(w: WarehouseState): WarehouseState =
    if (w.instructions.isEmpty) w else moveAll(move(w))

  @tailrec def moveBoxAll(w: WarehouseState): WarehouseState =
    if (w.instructions.isEmpty) w else moveBoxAll(moveBox(w))

  def moveBox(w: WarehouseState): WarehouseState =
    val nextDirection = Directions(w.instructions.head)
    val nextPoint = w.loc + nextDirection
    if (w.get(nextPoint) == Space ) WarehouseState(w.m.swap(w.loc, nextPoint), nextPoint, w.instructions.tail)
    else
      val box = if (isVertical(nextDirection)) getVerticalMovableBoxes(Set(nextPoint), nextDirection, w.m) else getHorizontalMovableBoxes(nextPoint, nextDirection, w.m)
      if (box.isEmpty) w.removeInstruction() else WarehouseState(shunt(w.m, box, nextDirection).update(nextPoint, Robot).update(w.loc, Space), nextPoint, w.instructions.tail)


  def shunt(m: Matrix[Char], boxes: Seq[Point], dir: Point): Matrix[Char] =
    val movedMatrix = boxes.map(p => (p, m(p))).foldLeft(m) { (i, t) =>
      i.update(t._1 + dir, t._2)
    }
    boxes.filterNot(boxes.map(_ + dir).contains).foldLeft(movedMatrix) { (o, v) =>
      o.update(v, Space)
    }

  @tailrec def getHorizontalMovableBoxes(x: Point, nav: Point, m: Matrix[Char], box: Seq[Point] = Seq()): Seq[Point] =
    if (isBox(m(x))) getHorizontalMovableBoxes(x + nav, nav, m, box :+ x)
    else if (m(x) == Wall) Seq()
    else box


  @tailrec def getVerticalMovableBoxes(x: Set[Point], nav: Point, m: Matrix[Char], box: Set[Point] = Set()): Seq[Point] =
    if (x.isEmpty) box.toSeq
    else
      val next = x.head
      m(next) match
        case BoxLeft => getVerticalMovableBoxes(x.tail + (next + nav) + (next + nav + Nav.EAST), nav, m, box + next + (next + Nav.EAST))
        case BoxRight => getVerticalMovableBoxes(x.tail + (next + nav) + (next + nav + Nav.WEST), nav, m, box + next + (next + Nav.WEST))
        case Wall => Seq()
        case _ => getVerticalMovableBoxes(x.tail, nav, m, box)


  def isVertical(p: Point): Boolean =
    p == Nav.NORTH || p == Nav.SOUTH

  def isBox(c: Char): Boolean =
    c == BoxLeft || c == BoxRight

  def move(w: WarehouseState): WarehouseState =
    val nextDirection = Directions(w.instructions.head)
    val nextPoint = w.loc + nextDirection
    val nextValue = w.get(nextPoint)

    if (nextValue == Space ) WarehouseState(w.m.swap(w.loc, nextPoint), nextPoint, w.instructions.tail)
    else if (nextValue == Wall) w.removeInstruction()
    else
      val o = findNextSpace(nextPoint, nextDirection, w.m)
      if (o.isEmpty) w.removeInstruction() else WarehouseState(w.m.swap(o.get, nextPoint).swap(w.loc, nextPoint), nextPoint, w.instructions.tail)


  @tailrec def findNextSpace(x: Point, nav: Point, m: Matrix[Char]): Option[Point] =
    val next = x + nav
    if (m.outRange(next) || (m(next) == Wall) ) None
    else if (m(next) == Space) Some(next)
    else findNextSpace(next, nav, m)

}
