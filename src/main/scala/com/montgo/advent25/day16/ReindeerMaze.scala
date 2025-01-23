package com.montgo.advent25.day16

import com.montgo.advent25.util.{Matrix, Nav, Point}

import scala.annotation.tailrec


case class Visited(point: Point, dir: Point)
case class Trip(position: Visited, score: Long = 0, path: Seq[Point] = Seq())
case class MazeState(m: Matrix[Char], visited: Map[Visited, Long] = Map())
case class MazeNav(trips: Set[Trip], ends: Set[Trip], destination: Point, state: MazeState)

object ReindeerMaze {
  def read(in: Seq[String]): MazeState =
    MazeState(Matrix(in.map(_.toVector).toVector))

  def navigate(state: MazeState): MazeNav =
    val start = Trip(Visited(state.m.find(_ == 'S').head, Nav.EAST))
    val init = MazeNav(Set(start), Set(), state.m.find(_ == 'E').head, state.copy(visited = state.visited + (start.position -> 0)))
    nav(init)

  @tailrec def nav(in: MazeNav): MazeNav =
    if (in.trips.isEmpty)
      in
    else
      val next = in.trips.head
      if (next.position.point == in.destination)
        nav(MazeNav(in.trips.tail, in.ends + next, in.destination, in.state))
      else
        val steps = nextSteps(in.trips.head, in.state)
        nav(MazeNav(in.trips.tail ++ steps, in.ends, in.destination, MazeState(in.state.m, in.state.visited ++ steps.map(s => s.position -> s.score))))

  def nextSteps(trip: Trip, s: MazeState): Seq[Trip] =
    Nav.ADJACENT_DIRS
      .filterNot(_ == trip.position.dir.perform(_ * -1))  // don't want to return previous
      .map(d => (d + trip.position.point, d))  // calc new position with dir
      .filter(t => s.m.inRange(t._1))
      .filterNot(t => s.m(t._1) == '#')  //  filter wall position
      .map(t => Visited(t._1, t._2))
      .map(v => Trip(v, trip.score + 1 + turnScore(trip.position.dir, v.dir), trip.path :+ trip.position.point))// map to new visited
      .filter(t => !s.visited.contains(t.position) || s.visited.get(t.position).exists(_ >= t.score))  // filter already visited
       // as trip

  private def turnScore(a: Point, b: Point): Long =
    if (Math.abs(a.col) == Math.abs(b.row)) 1000L else 0
}
