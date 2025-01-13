package com.montgo.advent25.day11

import com.montgo.advent25.util.MapUtil

import scala.concurrent.ExecutionContext.Implicits.global
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


case class StoneRule(q: BigInt => Boolean, f: BigInt => Seq[BigInt])

object Stones {

  private val RULES: Seq[StoneRule] = Seq(
    StoneRule(_ == 0L, _ => Seq(1L)),
    StoneRule(x => x.toString.length % 2 == 0, split),
    StoneRule(_ => true, x => Seq(x * 2024L))
  )

  def expandAll(in: Seq[BigInt], blinks: BigInt): (BigInt, Map[(BigInt, BigInt), BigInt]) =
    in.foldLeft((BigInt(0), Map[(BigInt, BigInt), BigInt]())) { (m, x) =>
      val res = expand(x, blinks, m._2)
      (res._1 + m._1, res._2)
    }

  def expand(n: BigInt, b: BigInt, m: Map[(BigInt, BigInt), BigInt] = Map()): (BigInt, Map[(BigInt, BigInt), BigInt]) =
    if b == 0 then
      (1, m)
    else
      if m.contains((n, b)) then
        (m((n, b)), m)
      else if n == 0 then
        val res = expand(1, b -1, m)
        (res._1, res._2 + ((n,b) -> res._1))
      else if n.toString.length % 2 == 0 then
        val nums = split(n)
        val first = expand(nums.head, b -1, m)
        val second = expand(nums(1), b -1, first._2)
        (first._1 + second._1, second._2 + ((n, b) -> (first._1 + second._1)))
      else
        val res = expand(n * 2024, b -1, m)
        (res._1, res._2 + ((n, b) -> res._1))

  def repeatIterate(iterations: Int, in: Seq[BigInt], c: Map[BigInt, Map[BigInt, BigInt]], result: Map[BigInt, BigInt] = Map()): (Map[BigInt, BigInt], Seq[BigInt]) =
    val res = iterate(iterations, in, c)
    if iterations == 0 || res._2.isEmpty then
        res
    else
      repeatIterate(iterations - 1, res._2, c, MapUtil.mapMerge(result, res._1, _+_))


  def iterate(iterations: Int, in: Seq[BigInt], c: Map[BigInt, Map[BigInt, BigInt]]): (Map[BigInt, BigInt], Seq[BigInt]) =
    println(s"Starting iterations for ${in.length} items")
    val f = Await.result(Future.sequence(in.map(l => Future(iteratePattern(2, l, c)))), Duration.Inf)
    val merged = f.foldLeft(Map[BigInt, BigInt]()) { (i, x) => MapUtil.mapMerge(i, x._1, _+_)}
    val remaining = f.foldLeft(Seq[BigInt]()) { (i,o) => i ++ o._2}
    (merged, remaining)

  def iteratePattern(iterations: Int, in: BigInt, c: Map[BigInt, Map[BigInt, BigInt]]): (Map[BigInt, BigInt], Seq[BigInt]) =
    println(s"start $in")
    val pattern = blinkAll(blinks(read(in.toString), 8), 17)
    val parsable = pattern.filter(c.keys.toSeq.contains)
    (iterateMap(iterations, asMap(parsable), c), pattern.filter(x => !c.keys.toSeq.contains(x)))

  @tailrec
  def iterateMap(iterations: Int, init: Map[BigInt, BigInt], c: Map[BigInt, Map[BigInt, BigInt]]): Map[BigInt, BigInt] =
    if (iterations == 0)
      init
    else
      val result = init.foldLeft(Map[BigInt, BigInt]()) { (i, x) =>
        val res = c.get(x._1).map(m =>
          m.map(t =>
            (t._1, t._2 * x._2)
          )
        ).getOrElse(i)
        MapUtil.mapMerge(i, res, _ + _)
      }
      iterateMap(iterations -1, result, c)

  /**
   * Find all convergent numbers using zero and then
   * calculate all the resulting instances for those convergent numbers
   * after 25 blinks
   *
   * @return
   */
  def getConvergenceMap(): Map[BigInt, Map[BigInt, BigInt]] =
    getZeroPatternMap().keys.map(k => (k, getPatternMap(k))).toMap

  def getZeroPatternMap(): Map[BigInt, BigInt] =
    getPatternMap(0L)

  //  25 iterations converges on 54 unique numbers
  def getPatternMap(in: BigInt): Map[BigInt, BigInt] =
    MapUtil.toMap(
      blinkAll(blinks(read(in.toString), 8), 17),
      BigInt(1), (_, v) => v + BigInt(1)
    )

  def asMap(in: Seq[BigInt]): Map[BigInt, BigInt] =
    MapUtil.toMap(in, BigInt(1), (_, v) => v + BigInt(1))

  def blinkAllSum(in:Seq[BigInt], count: Int): BigInt =
    blinkAll(in, count).length

  def blinkAll(in:Seq[BigInt], count: Int): Seq[BigInt] =
    Await.result(
      Future.sequence( in.map(x => Future(blinks(Seq(x), count)))),
      Duration.Inf
    ).flatten


  @tailrec def blinks(in:Seq[BigInt], count: BigInt): Seq[BigInt] =
    if (count == 0) in else blinks(blink(in), count-1)

  def blink(in:Seq[BigInt]): Seq[BigInt] =
    in.foldLeft(Seq[BigInt]()) { (i, v) =>
      i ++ RULES.find(r => r.q(v)).map(r => r.f(v)).getOrElse(Seq())
    }

  def read(in: String): Seq[BigInt] = in.split(' ').map(_.toLong).map(BigInt.long2bigInt)

  def split(in:BigInt): Seq[BigInt] =
    val s = in.toString
    val parts = s.splitAt(s.length / 2)
    Seq(parts._1.toLong, parts._2.toLong)

}
