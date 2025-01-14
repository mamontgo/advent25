package com.montgo.advent25.day12

import org.scalatest.funsuite.AnyFunSuite
import Garden.*
import com.montgo.advent25.util.Files

class GardenTest extends AnyFunSuite {


  val EXAMPLE = "./src/main/resources/day12/example.txt"
  val EXAMPLE2 = "./src/main/resources/day12/example2.txt"
  val EXAMPLE3 = "./src/main/resources/day12/example3.txt"
  val EXAMPLE4 = "./src/main/resources/day12/example4.txt"
  val EXAMPLE5 = "./src/main/resources/day12/example5.txt"
  val INPUT = "./src/main/resources/day12/input.txt"



  test("part two example one") {
    val m = read(Files.lines(EXAMPLE))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    assert(result == 80)
  }

  test("part two example two") {
    val m = read(Files.lines(EXAMPLE2))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    assert(result == 436)
  }

  test("part two example three") {
    val m = read(Files.lines(EXAMPLE3))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    assert(result == 1206)
  }

  test("part two example four") {
    val m = read(Files.lines(EXAMPLE4))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    assert(result == 236)
  }


  test("part two example five") {
    val m = read(Files.lines(EXAMPLE5))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    assert(result == 368)
  }

  test("part two input") {
    val m = read(Files.lines(INPUT))
    val result = collectAllGardenStatsPart2(m).map(t => t._1 * t._2).sum
    println(result)
    assert(result == 821428)
  }

  test("part one example one") {
    val m = read(Files.lines(EXAMPLE))
    val result = collectAllGardenStats(m).map(t => t._1 * t._2).sum
    assert(result == 140)
  }

  test("part one example two") {
    val m = read(Files.lines(EXAMPLE2))
    val result = collectAllGardenStats(m).map(t => t._1 * t._2).sum
    assert(result == 772)
  }

  test("part one example three") {
    val m = read(Files.lines(EXAMPLE3))
    val result = collectAllGardenStats(m).map(t => t._1 * t._2).sum
    assert(result == 1930)
  }

  test("part one input") {
    val m = read(Files.lines(INPUT))
    val result = collectAllGardenStats(m).map(t => t._1 * t._2).sum
    assert(result == 1431316)
  }

}
