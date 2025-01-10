package com.montgo.advent25.day9

import org.scalatest.funsuite.AnyFunSuite
import Fragmenter.*
import com.montgo.advent25.day9.DiskItem.{FileBlock, Space}
import com.montgo.advent25.util.Files

class FragmenterTest extends AnyFunSuite {

  val EXAMPLE = "./src/main/resources/day9/example.txt"
  val INPUT = "./src/main/resources/day9/input.txt"

  test("part 1 example init line") {
    val s = asDiskSeq(read(Seq("12345")).head)
    assert(checksum(swap(s))==60)
  }

  test("part 1 example file line") {
    val s = asDiskSeq(read(Files.lines(EXAMPLE)).head)
    assert(checksum(swap(s)) == 1928)
  }

  ignore("part 1 input file line") {
    val s = asDiskSeq(read(Files.lines(INPUT)).head)
    val result = checksum(swap(s))
    assert(result == 6201130364722L)
  }


  test("part 2 example compact ordering") {
    val c = compact(positions(read(Files.lines(EXAMPLE)).head))
    val cs = c.map(_.checksum).sum
    assert(cs == 2858)
  }


  test("part 2 input compact ordering") {
    val c = compact(positions(read(Files.lines(INPUT)).head))
    val cs = c.map(_.checksum).sum
    assert(cs == 6221662795602L)


    // 6221662795602 is our number
     // 5043865740101 to low
     // 5027661887314 to low
     // 6201130364722L
  }

}
