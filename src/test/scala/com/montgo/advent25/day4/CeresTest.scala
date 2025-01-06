package com.montgo.advent25.day4

import com.montgo.advent25.util.{Files, Point}
import org.scalatest.funsuite.AnyFunSuite
import Ceres.*

class CeresTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day4/example.txt"
  val INPUT = "./src/main/resources/day4/input.txt"

  test("part 1 ceres example") {
    val in = read(Files.lines(EXAMPLE))
    assert(countOccurrences("XMAS", in) == 18)
  }

  test("part 1 ceres input") {
    val in = read(Files.lines(INPUT))
    assert(countOccurrences("XMAS", in) == 2397)
  }

  test("part 2 example ceres xmas diagonal") {
    val in = read(Files.lines(EXAMPLE))
    assert(countXmasDiagonals(in) == 9)
  }

  test("part 2 input ceres xmas diagonal") {
    val in = read(Files.lines(INPUT))
    assert(countXmasDiagonals(in) == 1824)
  }

}
