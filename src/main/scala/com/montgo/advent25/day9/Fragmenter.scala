package com.montgo.advent25.day9

import com.montgo.advent25.day9.DiskItem.{FileBlock, Space}
import com.montgo.advent25.day9.DiskSeq.{FileSeq, SpaceSeq}
import com.montgo.advent25.util.FunctionUtil

import scala.annotation.tailrec

enum DiskSeq {

  case FileSeq(id: Long)
  case SpaceSeq

  def isSpace: Boolean = !this.isInstanceOf[FileSeq]
}

enum DiskItem {
  case FileBlock(blocks: Long, id: Long, position: Long = 0)
  case Space(blocks: Long, position: Long = 0)

  def isSpace: Boolean = this.isInstanceOf[Space]

  def print: String = this match
    case FileBlock(b, i, _) => i.toString * b.toInt
    case Space(b, _) => "." * b.toInt

  def checksum: Long = this match
    case FileBlock(b, i, p) => (p until p + b).map(_*i).sum
    case _ => 0L

}

object Fragmenter {

  def read(in: Seq[String]):Seq[Seq[DiskItem]] =
    in.map(l => l.tail.foldLeft( (Seq[DiskItem](FileBlock(l.head.asDigit, 0)), 1)) { (i, v) =>
      i._1.last match
        case FileBlock(_, _, _) => (i._1 :+ Space(v.asDigit), i._2)
        case Space(_,_) => (i._1 :+ FileBlock(v.asDigit, i._2), i._2+1)
    }._1)

  def positions(in: Seq[DiskItem]): Seq[DiskItem] =
    in.foldLeft((0L, Seq[DiskItem]())) { (i, v) =>
      v match
        case FileBlock(b, id, _) => (i._1 + b, i._2 :+ FileBlock(b, id, i._1))
        case Space(b, _) => (i._1 + b, i._2 :+ Space(b, i._1))
    }._2



  @tailrec def compact(in: Seq[DiskItem], unmovable: Seq[DiskItem] = Seq()): Seq[DiskItem] =
    val lastIndex = in.lastIndexWhere(!_.isSpace)
    if lastIndex < 0 then
      in ++ unmovable
    else
      val lastValue: FileBlock = in(lastIndex).asInstanceOf[FileBlock]
      val firstIndex = in.indexWhere(e => e.isSpace && e.asInstanceOf[Space].blocks >= lastValue.blocks)
      if firstIndex > lastIndex || firstIndex < 0 then
        // split at the last index
        val parts = in.splitAt(lastIndex)
        compact(parts._1, parts._2 ++ unmovable)
      else
        // replace the last with spaces
        val updated = in.updated(lastIndex, Space(lastValue.blocks, lastValue.position))
        // split at the first index
        val parts = updated.splitAt(firstIndex)
        val space: Space = parts._2.head.asInstanceOf[Space]
        val newFileBlock: FileBlock = FileBlock(lastValue.blocks, lastValue.id, space.position)
        val newSpace: Space = Space(space.blocks - newFileBlock.blocks, space.position + newFileBlock.blocks)
        if newSpace.blocks == 0 then
          compact(parts._1 ++ Seq(newFileBlock) ++ parts._2.tail, unmovable)
        else
          compact(parts._1 ++ Seq(newFileBlock, newSpace) ++ parts._2.tail, unmovable)

  def asDiskSeq(in: Seq[DiskItem]): Seq[DiskSeq] =
    in.flatMap {
      case FileBlock(c, id, _) => FunctionUtil.repeat(FileSeq(id), c.toInt)
      case Space(c,_) => FunctionUtil.repeat(SpaceSeq, c.toInt)
    }

  @tailrec def swap(in: Seq[DiskSeq]):  Seq[DiskSeq] =
    val lastIndex = in.lastIndexWhere(!_.isSpace)
    val firstIndex = in.indexWhere(_.isSpace)
    if firstIndex > lastIndex then
      in
    else
      val first = in(firstIndex)
      val last = in(lastIndex)
      swap(in.updated(firstIndex, last).updated(lastIndex, first))

  def checksum(in: Seq[DiskSeq]): Long =
    in.zipWithIndex.map { (v, i) =>
      v match
        case FileSeq(id) => id.toLong * i.toLong
        case _ => 0L
    }.sum




}
