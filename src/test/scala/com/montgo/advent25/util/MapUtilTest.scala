package com.montgo.advent25.util

import org.scalatest.funsuite.AnyFunSuite

class MapUtilTest extends AnyFunSuite {

  test("map merge") {
    val res = MapUtil.mapMerge(Map(1 -> 1, 2 -> 2), Map(2 -> 2, 3 -> 3), _+_)
    assert(res == Map(1 -> 1, 2 -> 4, 3 -> 3))
  }
}
