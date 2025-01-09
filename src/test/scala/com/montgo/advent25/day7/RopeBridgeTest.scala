package com.montgo.advent25.day7

import org.scalatest.funsuite.AnyFunSuite
import RopeBridge.{read, *}
import com.montgo.advent25.util.Files

class RopeBridgeTest extends AnyFunSuite {


  val EXAMPLE = "./src/main/resources/day7/example.txt"
  val INPUT = "./src/main/resources/day7/input.txt"

  test("part 1 example get calibrated values") {
    val in = read(Files.lines(EXAMPLE))
    val res = getCalibrated(in, OPS).map(_.value).sum
    assert(res == 3749)
  }

  test("part 1 input get calibrated values") {
    val in = read(Files.lines(INPUT))
    val res = getCalibrated(in, OPS).map(_.value).sum
    assert(res == 3598800864292L)
  }


  test("part 2 example get calibrated values") {
    val in = read(Files.lines(EXAMPLE))
    val res = getCalibrated(in, OPS2).map(_.value).sum
    assert(res == 11387)
  }

  test("part 2 input get calibrated values") {
    val in = read(Files.lines(INPUT))
    val res = getCalibrated(in, OPS2).map(_.value).sum
    assert(res == 340362529351427L)
  }
}
