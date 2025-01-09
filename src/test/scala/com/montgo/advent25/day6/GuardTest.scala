package com.montgo.advent25.day6

import com.montgo.advent25.util.{Files, Matrix, Point}
import org.scalatest.funsuite.AnyFunSuite
import com.montgo.advent25.pay6.Guard.*
import com.montgo.advent25.pay6.Position
import com.montgo.advent25.pay6.Position.{Obstacle, Space}
import com.montgo.advent25.util.Matrix.*
import com.montgo.advent25.util.MatrixVector

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


class GuardTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day6/example.txt"
  val INPUT = "./src/main/resources/day6/input.txt"


  test("part 1 example guard steps") {
    val m = asMatrix(Files.lines(EXAMPLE))
    val steps = findSteps(m)
    val set = steps.map(_.point).toSet
    assert(set.size == 41)
    val r = toChar(m.data)
    r.printMatrixStyle((x:Point) => if set.contains(x) then "31" else "0")

  }

  test("part 1 input guard steps") {
    val m = asMatrix(Files.lines(INPUT))
    val steps = findSteps(m)
    val set = steps.map(_.point).toSet
    assert(set.size == 4988)
  }

  test("part 2 example guard steps new obstacles") {
    val m = asMatrix(Files.lines(EXAMPLE))
    val positions = findObstaclePositions(m)
    assert(positions.length == 6)

  }

  ignore("part 2 input guard steps new obstacles") {
    val m = asMatrix(Files.lines(INPUT))
    val positions = findObstaclePositions(m)
    assert(positions.length == 1697)
  }

  ignore("part 2 input use existing path steps and calc each with future") {
    val f: Future[Seq[Boolean]] = Future.sequence(findObstaclePositionsTest(asMatrix(Files.lines(INPUT))))
    val res = Await.result(f, Duration.Inf)
    val x = res.count(p => p)
    assert(x == 1697)
    println(s"obstacles $x")

  }

  def findObstaclePositionsTest(m: Matrix[Position]): Seq[Future[Boolean]] =
    val start = getStart(m)
    val c = findSteps(m).map(_.point).filter(p => m.get(p) match {
      case Space => true
      case _ => false
    }).distinct
    c.map(p => Future( doWalk(m.update(p, Obstacle), start)._2))

  def toChar(m: MatrixVector[Position]): MatrixVector[Char] = {
    m.map(r => r.map {
      case Position.Obstacle => 'O'
      case Position.Direction(_) => '^'
      case _ => 'S'
    })
  }
}
