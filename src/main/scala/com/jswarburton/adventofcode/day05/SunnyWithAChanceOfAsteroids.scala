package com.jswarburton.adventofcode.day05

import com.jswarburton.adventofcode.intcode.IntCode

import scala.io.Source

object SunnyWithAChanceOfAsteroids {

  def puzzle1(filePath: String): List[Long] = run(read(filePath), inputInstruction = 1)

  def puzzle2(filePath: String): List[Long] = run(read(filePath), inputInstruction = 5)

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

  def run(data: List[Long], inputInstruction: Long): List[Long] = IntCode.runIntCode(data, List(inputInstruction))
}
