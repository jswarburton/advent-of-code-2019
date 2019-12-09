package com.jswarburton.adventofcode.day09

import com.jswarburton.adventofcode.intcode.IntCode

import scala.io.Source

object SensorBoost {

  def puzzle1(filePath: String): List[Long] = runBoost(read(filePath), input = 1)

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

  def runBoost(program: List[Long], input: Int): List[Long] = IntCode.runIntCode(program, List(input))

}
