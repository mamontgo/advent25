package com.montgo.advent25.util

import scala.annotation.targetName

type MatrixVector[T] = Vector[Vector[T]]

object Nav {
  val EAST: Point = Point(0, 1)
  val WEST: Point = Point(0, -1)
  val NORTH: Point = Point(-1, 0)
  val SOUTH: Point = Point(1, 0)

  val NORTH_WEST: Point = Point(-1, -1)
  val NORTH_EAST: Point = Point(-1, 1)
  val SOUTH_WEST: Point = Point(1, -1)
  val SOUTH_EAST: Point = Point(1, 1)


  val ADJACENT_DIRS: Seq[Point] = Seq(EAST, WEST, SOUTH, NORTH)
}

case class Point(row: Long, col: Long) {
  def up: Point = Point(row-1, col)
  def down: Point = Point(row+1, col)
  def left: Point = Point(row, col-1)
  def right: Point = Point(row, col+1)

  def map(f: Point => Point): Point = f(this)

  @`inline`
  @targetName("add")
  def +(o: Point): Point = Point(row + o.row, col + o.col)

  @`inline`
  @targetName("subtract")
  def -(o: Point): Point = Point(row - o.row, col - o.col)

  def invert: Point = Point(col, row)
}


case class Matrix[T](data: MatrixVector[T]) {
  lazy val width: Int = data.head.length
  lazy val height: Int = data.length
  def apply(p: Point): T = get(p)
  def get(row: Long, col:Long): T = data(row.toInt)(col.toInt)
  def get(p:Point): T = get(p.row, p.col)
  def inRange(row: Long, col:Long): Boolean = row >= 0 && row < height && col >= 0 && col < width
  def inRange(p:Point): Boolean = inRange(p.row, p.col)
  def outRange(p:Point): Boolean = !inRange(p.row, p.col)
  def value(p:Point): Option[T]  = if inRange(p) then Some(get(p)) else None
  def update(p:Point, v: T): Matrix[T] = Matrix(data.updated(p.row.toInt, data(p.row.toInt).updated(p.col.toInt, v)))
  def swap(x:Point, y:Point): Matrix[T] =
    val xVal = get(x)
    this.update(x, get(y)).update(y, xVal)
  def find(f: T => Boolean): Seq[Point] =
    (0 until width).foldLeft(Seq[Point]()) { (i, c) =>
      i ++ (0 until height).foldLeft(Seq[Point]()) { (i2, r) =>
        if f(get(r, c)) then i2 :+ Point(r, c) else i2
      }
    }

}

object Matrix {
  extension (obj: MatrixVector[Any]) {

    def printMatrixStyle(f: Point => String): Unit = {
      obj.zipWithIndex.foreach(row => {
        row._1.zipWithIndex.foreach(col => print(27.asInstanceOf[Char] + "[" + f(Point(row._2, col._2)) + "m" + col._1))
        println
      })
    }
  }
}


