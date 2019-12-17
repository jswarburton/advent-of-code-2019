package com.jswarburton.adventofcode.day16

import scala.annotation.tailrec
import scala.io.Source

object FlawedFrequencyTransmission {
  def read(filePath: String): List[Int] = Source.fromFile(filePath).getLines.toList.head.toList.map(_.asDigit)

  def fft(input: List[Int], numPhases: Int, basePattern: List[Int] = List(0, 1, 0, -1)): Int = {
    @tailrec
    def rec(latestInput: List[Int], currentPhase: Int = 0): List[Int] =
      if (currentPhase == numPhases) latestInput
      else {
        val inputLength = latestInput.length

        val result = for (i <- 0 until inputLength) yield {
          val expandedBasePattern = LazyList.continually(
            basePattern.flatMap(List.fill(i + 1)(_))).flatten.take(inputLength + 1).toList.tail

          latestInput
            .zip(expandedBasePattern)
            .map { case (i1, i2) => i1 * i2 }.sum.abs % 10
        }
        rec(result.toList, currentPhase + 1)
      }

    rec(input).take(8).foldLeft(0)((acc, i) => acc * 10 + i)
  }
}
