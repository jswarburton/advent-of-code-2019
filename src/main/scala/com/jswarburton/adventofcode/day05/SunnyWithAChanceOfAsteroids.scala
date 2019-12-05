package com.jswarburton.adventofcode.day05

import scala.annotation.tailrec
import scala.io.Source

sealed trait ParameterMode

case object PositionMode extends ParameterMode

case object ImmediateMode extends ParameterMode

object SunnyWithAChanceOfAsteroids {

  def puzzle1(filePath: String): List[Int] = run(read(filePath), inputInstruction = 1)

  def read(filePath: String): List[Int] =
    Source.fromFile(filePath).getLines.toList.head.split(",")
      .map(_.toInt)
      .toList

  def run(data: List[Int], inputInstruction: Int): List[Int] = {
    @tailrec
    def helper(latest: Vector[Int],
               inputInstructions: List[Int],
               outputs: List[Int] = List(),
               pointer: Int = 0): List[Int] = {

      val (opCode, mode1, mode2, _) = parseOpCodeAndModes(latest(pointer))

      def read(mode: ParameterMode, i: Int): Int = mode match {
        case PositionMode => latest(latest(pointer + i))
        case ImmediateMode => latest(pointer + i)
      }

      def write(i: Int, value: Int): Vector[Int] = latest.updated(latest(pointer + i), value)

      opCode match {
        // Add
        case 1 =>
          val newValue = read(mode1, 1) + read(mode2, 2)
          val newMemory = write(3, newValue)
          helper(latest = newMemory, inputInstructions = inputInstructions, outputs = outputs, pointer = pointer + 4)

        // Multiply
        case 2 =>
          val newValue = read(mode1, 1) * read(mode2, 2)
          val newMemory = write(3, newValue)
          helper(latest = newMemory, inputInstructions = inputInstructions, outputs = outputs, pointer = pointer + 4)

        // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
        // For example, the instruction 3,50 would take an input value and store it at address 50.
        case 3 =>
          val input :: newInputs = inputInstructions
          val newMemory = write(1, input)
          helper(latest = newMemory, pointer = pointer + 2, inputInstructions = newInputs, outputs = outputs)

        // Opcode 4 outputs the value of its only parameter.
        // For example, the instruction 4,50 would output the value at address 50.
        case 4 =>
          val newValue = read(mode1, 1)
          val newOutputs = newValue :: outputs
          helper(pointer = pointer + 2, outputs = newOutputs, inputInstructions = inputInstructions, latest = latest)

        // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 5 =>
          if (read(mode1, 1) != 0) helper(pointer = read(mode2, 2), outputs = outputs, inputInstructions = inputInstructions, latest = latest)
          else helper(pointer = pointer + 3, outputs = outputs, inputInstructions = inputInstructions, latest = latest)

        // Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 6 =>
          if (read(mode1, 1) == 0) helper(pointer = read(mode2, 2), outputs = outputs, inputInstructions = inputInstructions, latest = latest)
          else helper(pointer = pointer + 3, outputs = outputs, inputInstructions = inputInstructions, latest = latest)

        // Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 7 =>
          val newValue = if (read(mode1, 1) < read(mode2, 2)) 1 else 0
          val newMemory = write(3, newValue)
          helper(latest = newMemory, pointer = pointer + 4, outputs = outputs, inputInstructions = inputInstructions)

        // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 8 =>
          val newValue = if (read(mode1, 1) == read(mode2, 2)) 1 else 0
          val newMemory = write(3, newValue)
          helper(latest = newMemory, pointer = pointer + 4, outputs = outputs, inputInstructions = inputInstructions)

        case 99 => outputs
      }
    }

    helper(data.toVector, List(inputInstruction))
  }

  def parseOpCodeAndModes(value: Int): (Int, ParameterMode, ParameterMode, ParameterMode) = {
    val opCode = value % 100

    def mapToMode(i: Int): ParameterMode =
      i match {
        case 0 => PositionMode
        case 1 => ImmediateMode
      }

    val mode1 = value / 100 % 10
    val mode2 = value / 1000 % 10
    val mode3 = value / 10000 % 10

    (opCode, mapToMode(mode1), mapToMode(mode2), mapToMode(mode3))
  }

  def puzzle2(filePath: String): List[Int] = run(read(filePath), inputInstruction = 5)

}
