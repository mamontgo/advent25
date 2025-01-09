package com.montgo.advent25.day8

import com.montgo.advent25.util.{Files, MatrixVector, Point}
import org.scalatest.funsuite.AnyFunSuite
import com.montgo.advent25.util.Matrix.*
import Resonant.*

class ResonantTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day8/example.txt"
  val INPUT = "./src/main/resources/day8/input.txt"


  test("part 1 example count antinodes") {
    val in = read(Files.lines(EXAMPLE))
    val s = setAntinodePoints(in)
    assert(s.find(_.antinode).size == 14)
  }

  test("part 1 input count antinodes") {
    val in = read(Files.lines(INPUT))
    val s = setAntinodePoints(in)
    assert(s.find(_.antinode).size == 291)
  }

  test("part 2 example count all antinodes") {
    val in = read(Files.lines(EXAMPLE))
    val s = setAllAntinodePoints(in)
    val a = s.find(_.antinode)
    assert(a.size == 34)
//
//    val x = toChar(in.data)
//    x.printMatrixStyle((x:Point) => if a.contains(x) then "31" else "0")

  }


  test("part 2 input count all antinodes") {
    val in = read(Files.lines(INPUT))
    val s = setAllAntinodePoints(in)
    val a = s.find(_.antinode)
    println(a.size)
  }

  def toChar(m: MatrixVector[Antenna]): MatrixVector[Char] = {
    m.map(r => r.map(_.value).map {
      case Some(c) => c
      case _ => '.'
    })
  }
}
