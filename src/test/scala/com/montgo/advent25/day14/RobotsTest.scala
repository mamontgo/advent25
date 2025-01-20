package com.montgo.advent25.day14

import com.montgo.advent25.util.{Files, Matrix, Point}
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.ExecutionContext.Implicits.global
import Robots.*


class RobotsTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day14/example.txt"
  val INPUT = "./src/main/resources/day14/input.txt"

  val SMALL: Dimension = Dimension(10, 6)
  val LARGE: Dimension = Dimension(100, 102)


  test("pattern match") {
    val r = Room(LARGE, read(Files.lines(INPUT)))
    val c = ChristmasTree.findFirstPatter(r, ChristmasTree.SHAPE_ONE)
    assert(c == 7051)
    //    r.move(7051).prettyPrint()
//    (0 to 900000000).foldLeft(r) { (i, v) =>
//      val res = i.move
//
//        if (ChristmasTree.matchPattern(res.matrix, Robots.ROBOT_CHAR, ChristmasTree.SHAPE_ONE)) {
//          println(s" iteration $v+1")
//          println
//          println
//          res.prettyPrint()
//        }
//
//      res
//    }
  }


  test("part 1 example factor calc") {
    val r = Room(SMALL, read(Files.lines(EXAMPLE)))
    val f = r.move(100).factor
    assert(f == 12)
  }

  test("part 1 input factor calc") {
    val r = Room(LARGE, read(Files.lines(INPUT)))
    val f = r.move(100).factor
    assert(f == 229632480)

  }

}
