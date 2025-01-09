package com.montgo.advent25.day7

import scala.annotation.tailrec

case class Calibration(value: Long, params: Seq[Long])

type Op =  (Long, Long) => Long

object RopeBridge {


  val OPS: Seq[Op] = Seq[Op](_*_, _+_)
  val OPS2 = OPS :+ ((a,b) => a.toString.concat(b.toString).toLong)
  def read(in:Seq[String]): Seq[Calibration] =
    in.map(x =>
      val res = x.split(':')
      Calibration(res.head.trim.toLong, res.last.trim.split(' ').map(_.trim.toLong))
    )

  def getCalibrated(in: Seq[Calibration], ops: Seq[Op]):Seq[Calibration] = in.filter(hasOp(ops))

  def hasOp(ops: Seq[Op])(in: Calibration): Boolean =
    getOpCombos(ops)(in.params.tail, Seq(in.params.head)).contains(in.value)



  @tailrec def getOpCombos(ops: Seq[Op])(in: Seq[Long], results: Seq[Long]): Seq[Long] =
    if(in.isEmpty) results else getOpCombos(ops)(in.tail, results.flatMap(r => ops.map(op => op(r, in.head))))
}
