package com.montgo.advent25.day3

import org.scalatest.funsuite.AnyFunSuite
import TobogganShop.*
import com.montgo.advent25.util.Files

class TobogganShopTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day3/example.txt"
  val EXAMPLE2 = "./src/main/resources/day3/example2.txt"
  val INPUT = "./src/main/resources/day3/input.txt"

  test("Part 1 example sum matches") {
    assert(parseMatches(matchAll(Files.all(EXAMPLE))) == 161)
  }

  test("Part 1 input sum matches") {
    assert(parseMatches(matchAll(Files.all(INPUT))) == 178538786)
  }

  test("Part 2 example match all instructions") {
    val in = processInstructions(instructions(matchAllInstructions(Files.all(EXAMPLE2))))
    assert(in == 48)
  }

  test("Part 2 input match all instructions") {
    val in = processInstructions(instructions(matchAllInstructions(Files.all(INPUT))))
    assert(in == 102467299)
  }

}
