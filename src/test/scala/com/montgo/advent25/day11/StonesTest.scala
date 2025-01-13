package com.montgo.advent25.day11

import org.scalatest.funsuite.AnyFunSuite
import Stones.*
import java.util.Date
import scala.concurrent.{Await, Future}

class StonesTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day11/example.txt"
  val INPUT = "./src/main/resources/day11/input.txt"

  ignore("part one example 25 blinks") {
    val in = read("125 17")
    val s = Date().getTime
    val x = blinks(in, 25).size
    assert(x == 55312)

  }

  ignore("part one example 25 blinks with futures") {
    val in = read("125 17")
    val s = Date().getTime
    val x = blinkAll(in, 25)
    assert(x.size == 55312)
  }

  test("part one example 25 blinks with futures more pre blinks") {
    val in = read("125 17")
    val result = expandAll(in, 25)
    assert(result._1 == 55312)
  }


  test("part one input 25 blinks with futures more pre blinks") {
    val s = Date().getTime
    val in = read("8435 234 928434 14 0 7 92446 8992692")
    val result = expandAll(in, 25)
    assert(result._1 == 182081)
    println(new Date().getTime - s)
  }

  test("part two example 75 blinks with futures more pre blinks") {
    val in = read("125 17")
    val result = expandAll(in, 75)
    assert(result._1 == BigInt("65601038650482"))

  }

  test("part input example 75 blinks with futures more pre blinks") {
    val in = read("8435 234 928434 14 0 7 92446 8992692")
    val result = expandAll(in, 75)
    assert(result._1 == BigInt("216318908621637"))
  }






}
