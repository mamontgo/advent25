package com.montgo.advent25.day3

import com.montgo.advent25.day3.Instruction.{Processor, Values}

import scala.util.matching.Regex

enum Instruction {
  case Values(data:Seq[Long])
  case Processor(data: Boolean)
}

object TobogganShop {
  private val REGEX: Regex = "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)".r
  private val REGEX_EXCLUDE: Regex = "mul\\([0-9][0-9]?[0-9]?,[0-9][0-9]?[0-9]?\\)|do\\(\\)|don't\\(\\)".r

  def processInstructions(in: Seq[Instruction]): Long =
    in.foldLeft((true, 0L)) { (init, i) =>
      i match
        case Processor(x) => (x, init._2)
        case Values(x) => (init._1, if init._1 then init._2 + x.product else init._2)
    }._2

  def instructions(in: Seq[String]): Seq[Instruction] =
    in.map(toInstruction)

  def matchAllInstructions(in: String): Seq[String] =
    REGEX_EXCLUDE.findAllMatchIn(in).map(_.matched).toSeq

  def matchAll(in: String): Seq[String] =
    REGEX.findAllMatchIn(in).map(_.matched).toSeq

  def parseMatches(in: Seq[String]): Long =
    in.map(i => i.replaceAll("mul\\(", "").replaceAll("\\)", "").split(',').map(_.toLong).product).sum

  def toInstruction(in: String): Instruction =
    in.toLowerCase match
      case "don't()" => Processor(false)
      case "do()" => Processor(true)
      case a => Values(a.replaceAll("mul\\(", "").replaceAll("\\)", "").split(',').map(_.toLong))


}
