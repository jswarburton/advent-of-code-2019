package com.jswarburton.adventofcode.day07

import com.jswarburton.adventofcode.day07.AmplificationCircuit._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AmplificationCircuitTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day07/puzzle1and2-input.txt"

  behavior of "AmplificationCircuit"

  it should "produce correct thruster signal 1" in {
    val program = List(3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0)
    val phaseSequence = List(4, 3, 2, 1, 0)

    thrusterSignal(program, phaseSequence) shouldBe 43210
  }

  it should "produce correct thruster signal 2" in {
    val program = List(3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0)
    val phaseSequence = List(0, 1, 2, 3, 4)

    thrusterSignal(program, phaseSequence) shouldBe 54321
  }

  it should "produce correct thruster signal 3" in {
    val program = List(3, 31, 3, 32, 1002, 32, 10, 32, 1001, 31, -2, 31, 1007, 31, 0, 33, 1002, 33, 7, 33, 1, 33, 31, 31, 1, 32, 31, 31, 4, 31, 99, 0, 0, 0)
    val phaseSequence = List(1, 0, 4, 3, 2)

    thrusterSignal(program, phaseSequence) shouldBe 65210
  }

  it should "produce correct result for puzzle 1" in {
    val program = AmplificationCircuit.read(filePath)

    maxThrusterSignal(program) shouldBe 19650
  }

}
