package com.jswarburton.adventofcode.day07

import com.jswarburton.adventofcode.intcode.IntCode

import scala.io.Source

object AmplificationCircuit {
  def maxThrusterSignal(program: List[Long]): Long = {
    val phaseSequences = (0L to 4L).permutations.map(_.toList)
    phaseSequences.map(thrusterSignal(program, _)).max
  }

  def thrusterSignal(program: List[Long], phaseSequence: List[Long]): Long =
    phaseSequence.foldLeft(0L)((res, phase) => IntCode.runIntCode(program, List(phase, res)).head)

  def read(filePath: String): List[Long] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toLong)
      .toList

}
