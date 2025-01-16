package com.montgo.advent25.util

import scala.annotation.tailrec

object MathUtil {
  @tailrec final def gcd(a: BigInt, b: BigInt):BigInt=if (b==0) a.abs else gcd(b, a%b)
  def lcm(list: Seq[BigInt]):BigInt=list.foldLeft(BigInt(1))((a, b) => (a/gcd(a,b))*b)

}

case class Linear(slop: BigDecimal, intercept: BigDecimal)


object Linear {

  def fromPoints(a: (BigDecimal, BigDecimal), b: (BigDecimal, BigDecimal)): Linear =
    val slop = (b._2 - a._2) / (b._1 - a._1)
    Linear(slop, a._2 - slop * a._1)

  def intercept(a: Linear, b: Linear): (BigDecimal, BigDecimal) =
    val x  = (b.intercept - a.intercept)/(a.slop - b.slop)
    val x1  = (a.slop - b.slop) / (b.intercept - a.intercept)
    val y = (a.slop * x) + a.intercept
    (x,y)
}
