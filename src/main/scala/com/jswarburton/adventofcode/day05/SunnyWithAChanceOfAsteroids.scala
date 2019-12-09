package com.jswarburton.adventofcode.day05

import com.jswarburton.adventofcode.intcode.IntCode

import scala.io.Source

object SunnyWithAChanceOfAsteroids {

  def puzzle1(filePath: String): List[Int] = run(read(filePath), inputInstruction = 1)

  def puzzle2(filePath: String): List[Int] = run(read(filePath), inputInstruction = 5)

  def read(filePath: String): List[Int] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toInt)
      .toList

  def run(data: List[Int], inputInstruction: Int): List[Int] = IntCode.runIntCode(data, List(inputInstruction))
}
