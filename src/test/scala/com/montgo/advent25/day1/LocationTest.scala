package com.montgo.advent25.day1

import com.montgo.advent25.util.Files
import org.scalatest.funsuite.AnyFunSuite
import Location._

class LocationTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day1/example.txt"
  val INPUT = "./src/main/resources/day1/input.txt"


  test("parse input") {
    val in = parseInput(Files.lines(EXAMPLE))
    assert(in.length == 6)
  }

  test("arrange input") {
    val in = arrangeInput.compose(parseInput)(Files.lines(EXAMPLE))
    assert(in == (List(3, 4, 2, 1, 3, 3),List(4, 3, 5, 3, 9, 3)))
  }

  test("process input") {
    assert(processInput(Files.lines(EXAMPLE)) == (List(1, 2, 3, 3, 3, 4), List(3, 3, 3, 4, 5, 9)))
  }


  test("process example result part 1") {
    val in = distance(processInput(Files.lines(EXAMPLE)))
    assert(in == 11)
  }

  test("process input result part 1") {
    val in = distance(processInput(Files.lines(INPUT)))
    assert(in == 2344935)
  }


  test("process example similarity score part 2") {
    val in = similarity(processInput(Files.lines(EXAMPLE)))
    assert(in == 31)
  }


  test("process input similarity score part 2") {
    val in = similarity(processInput(Files.lines(INPUT)))
    assert(in == 27647262)
  }


}
