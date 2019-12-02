package com.jswarburton.adventofcode.day02

import scala.annotation.tailrec
import scala.io.Source

/**
 * 1202 Program Alarm
 */
object ProgramAlarm {
  def puzzle1(filePath: String): Int = {
    val data = read(filePath)
    val updated = data.toIndexedSeq.updated(1, 12).updated(2, 2).toList
    run(updated).head
  }

  def run(data: List[Int]): List[Int] = {
    def calculateNewIndexAndValue(seq: IndexedSeq[Int],
                                  i: Int,
                                  op: (Int, Int) => Int): (Int, Int) = {
      val firstIndex = seq(i + 1)
      val firstValue = seq(firstIndex)

      val secondIndex = seq(i + 2)
      val secondValue = seq(secondIndex)

      val destinationIndex = seq(i + 3)

      (destinationIndex, op(firstValue, secondValue))
    }

    @tailrec
    def helper(latest: IndexedSeq[Int], nextIndex: Int): IndexedSeq[Int] = {
      if (nextIndex > latest.size) latest
      else {
        latest(nextIndex) match {
          case 1 =>
            val (destinationIndex, destinationValue) = calculateNewIndexAndValue(latest, nextIndex, _ + _)
            helper(latest.updated(destinationIndex, destinationValue), nextIndex + 4)

          case 2 =>
            val (destinationIndex, destinationValue) = calculateNewIndexAndValue(latest, nextIndex, _ * _)
            helper(latest.updated(destinationIndex, destinationValue), nextIndex + 4)

          case 99 => latest
        }
      }
    }

    helper(data.toIndexedSeq, 0).toList
  }

  def read(filePath: String): List[Int] = Source.fromFile(filePath).getLines.toList.head.split(",")
    .map(_.toInt)
    .toList
}
