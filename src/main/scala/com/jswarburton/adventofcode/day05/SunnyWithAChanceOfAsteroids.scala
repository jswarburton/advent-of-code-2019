package com.jswarburton.adventofcode.day05

import intcode.IntCode

import scala.io.Source

sealed trait ParameterMode

case object PositionMode extends ParameterMode

case object ImmediateMode extends ParameterMode

object SunnyWithAChanceOfAsteroids {

  def puzzle1(filePath: String): List[Int] = run(read(filePath), inputInstruction = 1)

  def parseOpCodeAndModes(value: Int): (Int, ParameterMode, ParameterMode, ParameterMode) = {
    val opCode = value % 100

    def mapToMode(i: Int): ParameterMode = i match {
      case 0 => PositionMode
      case 1 => ImmediateMode
    }

    val mode1 = value / 100 % 10
    val mode2 = value / 1000 % 10
    val mode3 = value / 10000 % 10

    (opCode, mapToMode(mode1), mapToMode(mode2), mapToMode(mode3))
  }

  def puzzle2(filePath: String): List[Int] = run(read(filePath), inputInstruction = 5)

  def read(filePath: String): List[Int] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toInt)
      .toList

  def run(data: List[Int], inputInstruction: Int): List[Int] = IntCode.runIntCode(data, inputInstruction)
}
