package com.jswarburton.adventofcode.day07

import intcode.IntCode

import scala.io.Source

object AmplificationCircuit {
  def maxThrusterSignal(program: List[Int]): Int = {
    val phaseSequence = List(0, 1, 2, 3, 4)
    val permutations = phaseSequence.permutations

    permutations.map(thrusterSignal(program, _)).max
  }

  def thrusterSignal(program: List[Int], phaseSequence: List[Int]): Int =
    phaseSequence.foldLeft(0)((res, phase) => IntCode.runIntCode(program, List(phase, res)).head)

  def read(filePath: String): List[Int] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toInt)
      .toList

}
