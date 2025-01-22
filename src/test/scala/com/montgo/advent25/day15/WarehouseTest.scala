package com.montgo.advent25.day15

import org.scalatest.funsuite.AnyFunSuite
import Warehouse.*
import com.montgo.advent25.util.{Files, Nav, Point}
import com.montgo.advent25.util.Matrix.*

class WarehouseTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day15/example.txt"
  val EXAMPLE_ONE = "./src/main/resources/day15/example1.txt"
  val INPUT = "./src/main/resources/day15/input.txt"



  test("part one example small sum") {
    val p = moveAll(read(Files.lines(EXAMPLE)))
    assert(p.score == 2028)
  }

  test("part one example larger sum") {
    val p = moveAll(read(Files.lines(EXAMPLE_ONE)))
    p.m.data.printMatrixStyle(_ => "0")
    assert(p.score == 10092)
  }

  test("part two example larger sum") {
    val p = moveBoxAll(read(Files.lines(EXAMPLE_ONE)).expand)
    assert(p.boxScore == 9021)
  }

  test("part two input larger sum") {
    val p = moveBoxAll(read(Files.lines(INPUT)).expand)
    assert(p.boxScore == 1458740)

  }

}
