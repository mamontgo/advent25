package com.montgo.advent25.util

type MatrixVector[T] = Vector[Vector[T]]

case class Point(row: Int, col: Int) {
  def up: Point = Point(row-1, col)
  def down: Point = Point(row+1, col)
  def left: Point = Point(row, col-1)
  def right: Point = Point(row, col+1)
}


case class Matrix[T](data: MatrixVector[T]) {
  lazy val width: Int = data.head.length
  lazy val height: Int = data.length

  def get(row: Int, col:Int): T = data(row)(col)
  def get(p:Point): T = get(p.row, p.col)
  def inRange(row: Int, col:Int): Boolean = row >= 0 && row < height && col >= 0 && col < width
  def inRange(p:Point): Boolean = inRange(p.row, p.col)
  def value(p:Point): Option[T]  = if inRange(p) then Some(get(p)) else None
  def update(p:Point, v: T): Matrix[T] = Matrix(data.updated(p.row, data(p.row).updated(p.col, v)))
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


