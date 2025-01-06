package com.montgo.advent25.util

type MatrixVector[T] = Vector[Vector[T]]

case class Point(row: Int, col: Int)


case class Matrix[T](data: MatrixVector[T]) {
  lazy val width: Int = data.head.length
  lazy val height: Int = data.length

  def get(row: Int, col:Int): T = data(row)(col)
  def get(p:Point): T = get(p.row, p.col)
  def inRange(row: Int, col:Int): Boolean = row >= 0 && row < height && col >= 0 && col < width
  def inRange(p:Point): Boolean = inRange(p.row, p.col)
  def value(p:Point): Option[T]  = if inRange(p) then Some(get(p)) else None

}