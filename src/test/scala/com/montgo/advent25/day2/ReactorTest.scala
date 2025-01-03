package com.montgo.advent25.day2

import com.montgo.advent25.day2.Reactor.*
import com.montgo.advent25.util.Files
import org.scalatest.funsuite.AnyFunSuite

class ReactorTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day2/example.txt"
  val INPUT = "./src/main/resources/day2/input.txt"

  test("example count safe part 1") {
    val x = countSafe(parseInput(Files.lines(EXAMPLE)))
    assert(x == 2)
  }

  test("input count safe part 1") {
    val x = countSafe(parseInput(Files.lines(INPUT)))
    assert(x == 524)
  }

  test("example count damper safe part 2") {
    val x = countDamperSafe(parseInput(Files.lines(EXAMPLE)))
    assert(x == 4)
  }

  test("input count damper safe part 2") {
    val x = countDamperSafe(parseInput(Files.lines(INPUT)))
    assert(x == 569)
  }

//  test("is damper safe") {
//    val in = parseInput(Files.lines(EXAMPLE))
//    val x = in(4)
//    println(x)
//    println(isSafe(x))
//    println(isDamperSafe(x))
//  }

}
