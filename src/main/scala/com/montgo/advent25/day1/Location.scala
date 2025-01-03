package com.montgo.advent25.day1



object Location {

  def similarity(in: (Seq[Int], Seq[Int])): Int =
    in._1.map(i => i * in._2.count(_==i)).sum

  def distance(in: (Seq[Int], Seq[Int])): Int =
    in._1.zip(in._2).map(i => if i._2 >= i._1 then i._2 - i._1 else i._1 - i._2).sum

  def processInput(o:Seq[String]): (Seq[Int], Seq[Int]) =
    val in = arrangeInput.compose(parseInput)(o)
    (in._1.sorted, in._2.sorted)

  def parseInput(o:Seq[String]): Seq[(Int, Int)] =
    o
      .map(_.strip().replaceAll("   ", " ").split(' '))
      .map(i => (i.head.toInt, i.tail.head.toInt))

  def arrangeInput(in: Seq[(Int, Int)]): (Seq[Int], Seq[Int]) =
    in.foldLeft((Seq[Int](), Seq[Int]())) {(init, i) => (init._1 :+ i._1, init._2 :+ i._2) }


}
