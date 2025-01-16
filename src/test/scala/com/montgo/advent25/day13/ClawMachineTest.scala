package com.montgo.advent25.day13

import org.scalatest.funsuite.AnyFunSuite
import ClawMachine.*
import ClawMachineReader.*
import com.montgo.advent25.util
import com.montgo.advent25.util.Files
import com.montgo.advent25.util.MathUtil.*



class ClawMachineTest extends AnyFunSuite {


  val EXAMPLE = "./src/main/resources/day13/example.txt"
  val INPUT = "./src/main/resources/day13/input.txt"

  /**
   * Button A: X+94, Y+34
   * Button B: X+22, Y+67
   * Prize: X=8400, Y=5400
   */
  test("part 1 example token cost") {
    val m = read(Files.lines(EXAMPLE))
    val res = m.map(cost).sum
    assert(res == 480)
  }

  test("part 1 input token cost") {
    val m = read(Files.lines(INPUT))
    val res = m.map(cost).sum
    assert(res == 40069)
  }


  test("part 1 example token cost with stepping solution") {
    val m = read(Files.lines(EXAMPLE))
    val res = m.map(matchData).filter(_.isDefined).map(_.get).map(findMatches(_)).map(matchCost)
    println(res)
    assert(res.sum == 480)
  }

  test("part 1 example token cost with intercept solution") {
    val m = read(Files.lines(EXAMPLE))
    val res = m.map(getIntercept).filter(_.isDefined).map(_.get)
    assert(comboCost(res).sum == 480)
  }

  test("part 1 input token cost with intercept solution") {
    val m = read(Files.lines(INPUT))
    val res = m.map(getIntercept).filter(_.isDefined).map(_.get)
    assert(comboCost(res).sum == 40069)
  }

  test("part 2 input token cost with intercept solution") {
    val c = BigInt("10000000000000")
    val m = read(Files.lines(INPUT)).map(m => m.copy(p = Prize(m.p.row + c, m.p.col + c)))
    val res = m.map(getIntercept).filter(_.isDefined).map(_.get)

    assert(comboCost(res).sum == 71493195288102L)
  }





}