package com.montgo.advent25.day10

import com.montgo.advent25.util.{Files, Point}
import org.scalatest.funsuite.AnyFunSuite
import TrailHead.*
class TrailHeadTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day10/example.txt"
  val INPUT = "./src/main/resources/day10/input.txt"

  test("part 1 example find hiking trails") {
    val m = read(Files.lines(EXAMPLE))
    val ends = getTrailHeads(m).map(h=> nav(m)(Seq(h)).toSet)
    val res = ends.map(_.size).sum
    assert(res == 36)
  }

  test("part 1 input find hiking trails") {
    val m = read(Files.lines(INPUT))
    val ends = getTrailHeads(m).map(h => nav(m)(Seq(h)).toSet)
    val res = ends.map(_.size).sum
    assert(res == 624)
  }


  test("part 2 example find hiking trails ratings") {
    val m = read(Files.lines(EXAMPLE))
    val ends = getTrailHeads(m).map(h => nav(m)(Seq(h)))
    val res = ends.map(_.size).sum
    assert(res == 81)
  }

  test("part 2 input find hiking trails ratings") {
    val m = read(Files.lines(INPUT))
    val ends = getTrailHeads(m).map(h => nav(m)(Seq(h)))
    val res = ends.map(_.size).sum
    assert(res == 1483)
  }

}
