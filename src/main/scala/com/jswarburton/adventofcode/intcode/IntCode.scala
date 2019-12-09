package com.jswarburton.adventofcode.intcode

import scala.annotation.tailrec

object IntCode {
  def runIntCode(data: List[Long], inputInstructions: List[Long]): List[Long] = {
    @tailrec
    def helper(latest: Map[Int, Long],
               inputInstructions: List[Long],
               outputs: List[Long] = List(),
               pointer: Int = 0,
               relativeBase: Int = 0): List[Long] = {

      val (opCode, mode1, mode2, mode3) = parseOpCodeAndModes(latest(pointer))

      def read(mode: ParameterMode, i: Int): Long = mode match {
        case PositionMode => latest(latest(pointer + i).toInt)
        case ImmediateMode => latest(pointer + i)
        case RelativeMode => latest(latest(pointer + i).toInt + relativeBase)
      }

      def write(mode: ParameterMode, i: Int, value: Long): Map[Int, Long] =
        mode match {
          case PositionMode => latest.updated(latest(pointer + i).toInt, value)
          case ImmediateMode => throw new IllegalArgumentException("Trying to write in immediate mode")
          case RelativeMode => latest.updated(latest(pointer + i).toInt + relativeBase, value)
        }

      opCode match {
        // Add
        case 1 =>
          val newValue = read(mode1, 1) + read(mode2, 2)
          val newLatest = write(mode3, 3, newValue)
          helper(latest = newLatest, inputInstructions = inputInstructions, outputs = outputs, pointer = pointer + 4, relativeBase = relativeBase)

        // Multiply
        case 2 =>
          val newValue = read(mode1, 1) * read(mode2, 2)
          val newLatest = write(mode3, 3, newValue)
          helper(latest = newLatest, inputInstructions = inputInstructions, outputs = outputs, pointer = pointer + 4, relativeBase = relativeBase)

        // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
        // For example, the instruction 3,50 would take an input value and store it at address 50.
        case 3 =>
          val input :: newInputs = inputInstructions
          val newLatest = write(mode1, 1, input)
          helper(latest = newLatest, pointer = pointer + 2, inputInstructions = newInputs, outputs = outputs, relativeBase = relativeBase)

        // Opcode 4 outputs the value of its only parameter.
        // For example, the instruction 4,50 would output the value at address 50.
        case 4 =>
          val output = read(mode1, 1)
          val newOutputs = output :: outputs
          helper(pointer = pointer + 2, outputs = newOutputs, inputInstructions = inputInstructions, latest = latest, relativeBase = relativeBase)

        // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 5 =>
          if (read(mode1, 1) != 0) helper(pointer = read(mode2, 2).toInt, outputs = outputs, inputInstructions = inputInstructions, latest = latest, relativeBase = relativeBase)
          else helper(pointer = pointer + 3, outputs = outputs, inputInstructions = inputInstructions, latest = latest, relativeBase = relativeBase)

        // Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
        case 6 =>
          if (read(mode1, 1) == 0) helper(pointer = read(mode2, 2).toInt, outputs = outputs, inputInstructions = inputInstructions, latest = latest, relativeBase = relativeBase)
          else helper(pointer = pointer + 3, outputs = outputs, inputInstructions = inputInstructions, latest = latest, relativeBase = relativeBase)

        // Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 7 =>
          val newValue = if (read(mode1, 1) < read(mode2, 2)) 1 else 0
          val newLatest = write(mode3, 3, newValue)
          helper(latest = newLatest, pointer = pointer + 4, outputs = outputs, inputInstructions = inputInstructions, relativeBase = relativeBase)

        // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
        case 8 =>
          val newValue = if (read(mode1, 1) == read(mode2, 2)) 1 else 0
          val newLatest = write(mode3, 3, newValue)
          helper(latest = newLatest, pointer = pointer + 4, outputs = outputs, inputInstructions = inputInstructions, relativeBase = relativeBase)

        // Opcode 9 adjusts the relative base by the value of its only parameter
        case 9 =>
          val relativeBaseAdjustment = read(mode1, 1).toInt
          helper(latest = latest, pointer = pointer + 2, outputs = outputs, inputInstructions = inputInstructions, relativeBase = relativeBase + relativeBaseAdjustment)

        case 99 => outputs.reverse
      }
    }

    val indexToValue = data.zipWithIndex.map { case (value, idx) => (idx, value) }.toMap.withDefaultValue(0L)

    helper(indexToValue, inputInstructions)
  }

  def parseOpCodeAndModes(value: Long): (Int, ParameterMode, ParameterMode, ParameterMode) = {
    val opCode = value % 100

    def mapToMode(i: Long): ParameterMode = i match {
      case 0 => PositionMode
      case 1 => ImmediateMode
      case 2 => RelativeMode
    }

    val mode1 = value / 100 % 10
    val mode2 = value / 1000 % 10
    val mode3 = value / 10000 % 10

    (opCode.toInt, mapToMode(mode1), mapToMode(mode2), mapToMode(mode3))
  }
}
