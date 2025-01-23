package com.montgo.advent25.day16

import org.scalatest.funsuite.AnyFunSuite
import ReindeerMaze.*
import com.montgo.advent25.util.Files
import com.montgo.advent25.util.Matrix.*

class ReindeerMazeTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day16/example.txt"
  val EXAMPLE_ONE = "./src/main/resources/day16/example1.txt"
  val INPUT = "./src/main/resources/day16/input.txt"

  test("part one example nav") {
    val s = read(Files.lines(EXAMPLE))
    val x = navigate(s)
    assert(x.ends.map(_.score).min == 7036)
  }

  test("part one example one nav") {
    val s = read(Files.lines(EXAMPLE_ONE))
    val x = navigate(s)
    assert(x.ends.map(_.score).min == 11048)
  }

  ignore("part one input nav") {
    val s = read(Files.lines(INPUT))
    val x = navigate(s)
    assert(x.ends.map(_.score).min == 82460)
  }


  test("part two example nav") {
    val s = read(Files.lines(EXAMPLE))
    val x = navigate(s)
    val min = x.ends.map(_.score).min
    val path = x.ends.filter(_.score==min).flatMap(_.path)
    assert(path.size + 1 == 45)
  }

  test("part two example one nav") {
    val s = read(Files.lines(EXAMPLE_ONE))
    val x = navigate(s)
    val min = x.ends.map(_.score).min
    val path = x.ends.filter(_.score == min).flatMap(_.path)
    assert(path.size + 1 == 64)
  }


  ignore("part two input nav") {
    val s = read(Files.lines(INPUT))
    val x = navigate(s)
    val min = x.ends.map(_.score).min
    val path = x.ends.filter(_.score == min).flatMap(_.path)
    assert(path.size + 1 == 590)
//    s.m.data.printMatrixStyle(p => if (path.contains(p)) "31" else "0")
  }

}
