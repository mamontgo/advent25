package com.montgo.advent25.day2

import com.montgo.advent25.util.FunctionUtil
import FunctionUtil._

enum Dir {
  case GREATER
  case LESSER
  case UNKNOWN
  case INIT
}



object Reactor {

  private val MAX_DIFF = 3

  def parseInput(in:Seq[String]): Seq[Seq[Long]] =
    in.map(i => i.trim.split(' ').map(_.toLong))

  def countSafe(in: Seq[Seq[Long]]): Long =
    in.map(isSafe).count(_==true)

  def countDamperSafe(in: Seq[Seq[Long]]): Long =
    in.map(x => isSafe(x) || isDamperSafe(x)).count(_ == true)


  def isDamperSafe(in: Seq[Long]): Boolean =
    val x = foldUntil(in.indices, (Seq[Long](), in, false), k => k._3 == true) { (init, i) =>
      val safe = isSafe(init._1 ++ init._2.tail)
      (init._1 :+ init._2.head, init._2.tail, safe)
    }
    x._3

  def isSafe(in: Seq[Long]): Boolean =
    val res = foldUntil(in, (Dir.INIT, true, in.head), k => k._2 == false) { (init, i) =>
      if i > init._3 then
          if init._1 == Dir.LESSER || i - init._3 > MAX_DIFF then
            (init._1, false, i)
          else
            (Dir.GREATER, true, i)
      else if i < init._3 then
        if init._1 == Dir.GREATER || init._3 - i > MAX_DIFF then
          (init._1, false, i)
        else
          (Dir.LESSER, true, i)
      else if init._1 == Dir.INIT then
        (Dir.UNKNOWN, true, i)  //  init is first item so ignore and continue with known
      else
        (Dir.UNKNOWN, false, i)  //  unknown means items are the same other than the first item so this fails
    }
    res._2
}

