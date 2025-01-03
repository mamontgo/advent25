package com.montgo.advent25.util

import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class InitTest extends AnyFunSuite {
  test("init hello test") {
    assert(Init.hello("mark") == "mark hello")
  }

  test("read file") {
    val source = Source.fromFile("./src/main/resources/test.txt")
    assert(source.getLines().toIndexedSeq.head == "Hello World")
  }
}
