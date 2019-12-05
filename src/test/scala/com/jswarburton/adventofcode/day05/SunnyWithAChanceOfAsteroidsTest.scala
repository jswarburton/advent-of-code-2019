package com.jswarburton.adventofcode.day05

import com.jswarburton.adventofcode.day05.SunnyWithAChanceOfAsteroids.{parseOpCodeAndModes, puzzle1, puzzle2}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SunnyWithAChanceOfAsteroidsTest extends AnyFlatSpec with Matchers {

  private val filePath = "src/main/resources/day05/puzzle1and2-input.txt"

  behavior of "SunnyWithAChanceOfAsteroids"

  it should "parse Op Codes and Modes" in {
    parseOpCodeAndModes(1002) shouldBe(2, PositionMode, ImmediateMode, PositionMode)
    parseOpCodeAndModes(10104) shouldBe(4, ImmediateMode, PositionMode, ImmediateMode)
  }

  it should "produce correct result for puzzle 1" in {
    val head :: tail = puzzle1(filePath)

    head shouldBe 13787043
    tail.forall(_ == 0) shouldBe true
  }
  it should "produce correct result for puzzle 2" in {
    val head :: tail = puzzle2(filePath)

    head shouldBe 3892695
    tail.forall(_ == 0) shouldBe true
  }

  it should "produce correct results for examples given" in {
    val positionModeEqualTo8 = List(3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8)
    evaluate(positionModeEqualTo8, inputInstruction = 8, expectedHead = 1)
    evaluate(positionModeEqualTo8, inputInstruction = 6, expectedHead = 0)

    val positionModeLessThan8 = List(3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8)
    evaluate(positionModeLessThan8, inputInstruction = 8, expectedHead = 0)
    evaluate(positionModeLessThan8, inputInstruction = 9, expectedHead = 0)
    evaluate(positionModeLessThan8, inputInstruction = 7, expectedHead = 1)

    val immediateModeEqualTo8 = List(3, 3, 1108, -1, 8, 3, 4, 3, 99)
    evaluate(immediateModeEqualTo8, inputInstruction = 8, expectedHead = 1)
    evaluate(immediateModeEqualTo8, inputInstruction = 6, expectedHead = 0)

    val immediateModeLessThan8 = List(3, 3, 1107, -1, 8, 3, 4, 3, 99)
    evaluate(immediateModeLessThan8, inputInstruction = 8, expectedHead = 0)
    evaluate(immediateModeLessThan8, inputInstruction = 9, expectedHead = 0)
    evaluate(immediateModeLessThan8, inputInstruction = 7, expectedHead = 1)
  }

  it should "produce correct result for larger example" in {
    val input = List(3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
      1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
      999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99)

    evaluate(input, inputInstruction = 7, expectedHead = 999)
    evaluate(input, inputInstruction = 8, expectedHead = 1000)
    evaluate(input, inputInstruction = 9, expectedHead = 1001)
  }

  it should "output 0 if input is zero, 1 otherwise" in {
    val inputUsingPositionMode = List(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)
    evaluate(inputUsingPositionMode, inputInstruction = 0, expectedHead = 0)
    evaluate(inputUsingPositionMode, inputInstruction = 5, expectedHead = 1)

    val inputUsingImmediateMode = List(3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1)
    evaluate(inputUsingImmediateMode, inputInstruction = 0, expectedHead = 0)
    evaluate(inputUsingImmediateMode, inputInstruction = 5, expectedHead = 1)
  }

  private def evaluate(input: List[Int], inputInstruction: Int, expectedHead: Int): Unit = {
    val res = SunnyWithAChanceOfAsteroids.run(data = input, inputInstruction = inputInstruction)

    res.head shouldBe expectedHead
    res.tail.forall(_ == 0) shouldBe true
  }
}
