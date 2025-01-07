package com.montgo.advent25.day5

import org.scalatest.funsuite.AnyFunSuite
import PagePrinter.*
import com.montgo.advent25.util.Files

class PagePrintTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day5/example.txt"
  val INPUT = "./src/main/resources/day5/input.txt"

  test("parse input test") {
    val in = parseInput(Files.lines(EXAMPLE))
    println(in)
  }


  test("sorting test") {
    val in = parseInput(Files.lines(EXAMPLE))
    val rules = parseRules(in._1)
    val invalid = filterInvalid(in._2, rules)
    println(invalid.map(_.sortWith(sorter(rules))))
    println(
      Seq(10,4,34,6,2,54,32).sortWith( (a,b) => a > b)
    )
  }

  test("parse rules as map") {
    val in = parseInput(Files.lines(EXAMPLE))
    println(parseRules(in._1))
  }

  test("part 1 example sum middle valid lines") {
    val in = parseInput(Files.lines(EXAMPLE))
    assert(getMiddle(filterValid(in._2, parseRules(in._1))).sum == 143)
  }

  test("part 1 input sum middle valid lines") {
    val in = parseInput(Files.lines(INPUT))
    assert(getMiddle(filterValid(in._2, parseRules(in._1))).sum == 6498)
  }

  test("part 2 example sum middle sorted invalid lines") {
    val in = parseInput(Files.lines(EXAMPLE))
    val rules = parseRules(in._1)
    assert(getMiddle(filterInvalid(in._2, rules).map(_.sortWith(sorter(rules)))).sum == 123)
  }

  test("part 2 input sum middle sorted invalid lines") {
    val in = parseInput(Files.lines(INPUT))
    val rules = parseRules(in._1)
    assert(getMiddle(filterInvalid(in._2, rules).map(_.sortWith(sorter(rules)))).sum == 5017)
  }
}
