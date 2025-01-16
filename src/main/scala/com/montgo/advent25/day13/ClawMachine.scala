package com.montgo.advent25.day13

import com.montgo.advent25.util.{Linear, Point, TupleUtil}
import Linear.*
import jdk.internal.joptsimple.internal.Strings

import scala.annotation.tailrec
import scala.math.BigDecimal.RoundingMode


case class Prize(row: BigInt, col: BigInt) {
  def invert: Prize = Prize(col, row)
}

case class Machine(a: Point, b: Point, p:Prize) {
  lazy val invert: Machine = Machine(a.invert, b.invert, p.invert)
  def switch: Machine = Machine(b, a, p)
}


object ClawMachine {

  private def getInitialSteps(m: Machine, i: BigDecimal): (BigDecimal, BigDecimal) =
    val mod = m.p.col % m.b.col
    val x = (i * m.b.col) + mod.toDouble
    (x / m.a.col, (m.p.col.toDouble - x) / m.b.col)

  def scaled(in: BigDecimal): BigDecimal = in.setScale(10, RoundingMode.HALF_EVEN)

  def getIntercept(m: Machine): Option[(BigInt, BigInt)] =

    val firstL: Linear = fromPoints(getInitialSteps(m, 1), getInitialSteps(m, 2))
    val secondL: Linear = fromPoints(getInitialSteps(m.invert, 1), getInitialSteps(m.invert, 2))
    val res: (BigDecimal, BigDecimal) = TupleUtil.map(intercept(firstL, secondL), scaled)
    if (res._1.isWhole && res._2.isWhole)
      val t = TupleUtil.map(res, _.toBigInt)
      if t._1 >= 0 && t._2 >=0 && t._1 <= m.p.col && t._2 <= m.p.row then
        Some(t)
      else
        None
    else
      None

  def comboCost(in: Seq[(BigInt, BigInt)]): Seq[BigInt] =
    in.map(t => (t._1 * 3) + t._2)

  def cost(m: Machine): BigInt =
    val c = allCombos(m)
    matchCost(c._1.filter(c._2.contains))

  def matchCost(in: Seq[(BigInt, BigInt)]): BigInt =
    val costs = comboCost(in)
    if (costs.isEmpty) 0L else costs.min


  def allCombos(m: Machine): (Seq[(BigInt,BigInt)], Seq[(BigInt,BigInt)]) =
    (combos(m), combos(m.invert))

  def combos(m: Machine): Seq[(BigInt,BigInt)] =
    getDividersA(m).zip(getDividersA(m.switch).reverse)

  def getDividersA(m: Machine): Seq[BigInt] =
    val mod = m.p.col % m.b.col
    val y = m.p.col / m.b.col
    val out = (0 to y.toInt).filter(x => ((x * m.b.col) + mod) % m.a.col == 0)
    out.map(x => ((x * m.b.col) + mod) / m.a.col)

  def matchData(m: Machine): Option[MatchData] =
    val steps = getFirstTwoSteps(m)
    if (steps.isEmpty) None
    else if (steps.length == 1) Some(MatchData(steps.head._1, steps.head._2, m.p.row, m.p.col, m))
    else Some(MatchData(steps.head._1, steps.head._2, steps.last._1 - steps.head._1, steps.last._2 - steps.head._2, m))

  def getFirstTwoSteps(m: Machine): Seq[(BigInt, BigInt)] =
    getFirstTwoStepsRange(m)

  def getFirstTwoStepsRange(m: Machine): Seq[(BigInt, BigInt)] =
    val mod = m.p.col % m.b.col

    Range.Long(0L, (m.p.col / m.b.col).toLong, 1).view.map(x => (x * m.b.col) + mod)
      .filter(_ % m.a.col == 0)
      .take(2).toSeq.map(x => (x/m.a.col, (m.p.col - x)/m.b.col))


  @tailrec def findMatches(m: MatchData, matches: Seq[(BigInt, BigInt)] = Seq() ):Seq[(BigInt, BigInt)] =
    if (!m.inRange || matches.nonEmpty) matches else findMatches(m.next, matches ++ m.matches)

}

case class MatchData(ay: BigInt, by: BigInt, aInc: BigInt, bInc: BigInt, machine: Machine) {
  private lazy val maxA: BigInt = machine.p.col / machine.a.col

  def inRange: Boolean = (by >=0) && (ay <= maxA)

  def next: MatchData = this.copy(ay = ay + aInc, by = by + bInc)

  def matches: Seq[(BigInt, BigInt)] = if (isMatch) Seq((ay, by)) else Seq()

  private def isMatch: Boolean = ((machine.a.row * ay) + (machine.b.row * by)) == machine.p.row

}

object ClawMachineReader {
  def read(in: Seq[String]): Seq[Machine] =
    readNext(in.filterNot(_ == Strings.EMPTY))

  @tailrec def readNext(in: Seq[String], out: Seq[Machine] = Seq()): Seq[Machine] =
    val parts = in.splitAt(3)
    val m = out :+ Machine(btn(parts._1.head), btn(parts._1(1)), prz(parts._1(2)))
    if (parts._2.isEmpty) m else readNext(parts._2, m)

  private val btn: String => Point = button compose coords
  private val prz: String => Prize = prize compose coords

  def button(in: Seq[String]): Point =
    Point(in.head.split('+').last.toInt, in(1).split('+').last.toInt)

  def prize(in: Seq[String]): Prize =
    Prize(BigInt(in.head.split('=').last), BigInt(in(1).split('=').last))

  def coords(in: String): Seq[String] =
    in.split(':')(1).split(',')
}