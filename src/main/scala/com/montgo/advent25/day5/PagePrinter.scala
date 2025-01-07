package com.montgo.advent25.day5

import scala.annotation.tailrec

enum DIR {
  case BEFORE
  case AFTER
}

type RuleInput = (Seq[(Int, Int)], Seq[Seq[Int]])
type RuleMap =  Map[Int, (Seq[Int], Seq[Int])]

case class PrintPoint(point: Int, before: Seq[Int], after: Seq[Int])

object PagePrinter {

  val EMPTY:(Seq[Int], Seq[Int]) = (Seq(), Seq())

  def parseInput(in:Seq[String]): (Seq[(Int, Int)], Seq[Seq[Int]]) = {
    in.foldLeft( Seq[(Int, Int)](), Seq[Seq[Int]]())  { (i, x) =>
      if x.contains('|') then
        val rule = x.split('|').map(_.toInt)
        (i._1 :+ (rule(0), rule(1)), i._2)
      else if x.contains(',') then
        (i._1 , i._2 :+ x.split(',').map(_.toInt))
      else
        i
    }
  }

  def parseRules(in: Seq[(Int, Int)]): RuleMap =
    in.foldLeft(Map[Int, (Seq[Int], Seq[Int])]()) { (i, x) =>
      i + i.get(x._1).map(v => x._1 -> (v._1, v._2 :+ x._2)).getOrElse(x._1 -> (Seq(), Seq(x._2)))
      + i.get(x._2).map(v => x._2 -> (v._1 :+ x._1, v._2)).getOrElse(x._2 -> (Seq(x._1), Seq()))
    }

  def sorter(rules: RuleMap): (Int, Int) => Boolean =
    (a, b) => rules.getOrElse(b, EMPTY)._1.contains(a) || rules.getOrElse(a, EMPTY)._2.contains(b)

  def filterValid(in: Seq[Seq[Int]], rules: RuleMap): Seq[Seq[Int]] =
    in.filter(x => validatePrintPoints(asPoint(x), rules))

  def filterInvalid(in: Seq[Seq[Int]], rules: RuleMap): Seq[Seq[Int]] =
    in.filter(x => !validatePrintPoints(asPoint(x), rules))

  def getMiddle(in: Seq[Seq[Int]]): Seq[Int] =
    in.map(x => x(x.length/2))

  private def asPoint(in:Seq[Int]): PrintPoint = PrintPoint(in.head, Seq(), in.tail)

  @tailrec private def validatePrintPoints(in:PrintPoint, rules: RuleMap): Boolean =
    if isInvalid(in, rules) then false
    else if in.after.isEmpty then true else validatePrintPoints(PrintPoint(in.after.head, in.before :+ in.after.head, in.after.tail), rules)

  private def isInvalid(point: PrintPoint, rules: RuleMap): Boolean =
    rules.get(point.point).map(r => r._1.exists(point.after.contains) || r._2.exists(point.before.contains) ).get
}
