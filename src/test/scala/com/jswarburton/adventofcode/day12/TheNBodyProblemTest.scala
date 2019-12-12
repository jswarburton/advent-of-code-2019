package com.jswarburton.adventofcode.day12

import com.jswarburton.adventofcode.day12.TheNBodyProblem.{calculateTotalEndEnergy, read}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TheNBodyProblemTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day12/puzzle1and2-input.txt"

  behavior of "The N-Body Problem"

  it should "read data correctly" in {
    read(filePath) shouldBe List(
      Position(x = 0, y = 6, z = 1),
      Position(x = 4, y = 4, z = 19),
      Position(x = -11, y = 1, z = 8),
      Position(x = 2, y = 19, z = 15))
  }

  it should "correctly calculate energies" in {
    val posAndVel = PositionAndVelocity(
      Position(x = -29, y = 11, z = -1),
      Velocity(x = -3, y = 7, z = 4))

    posAndVel.kineticEnergy shouldBe 14
    posAndVel.potentialEnergy shouldBe 41
    posAndVel.totalEnergy shouldBe 574
  }

  it should "correctly total energy after multiple iterations" in {
    val startingPositions = List(
      Position(x = -8, y = -10, z = 0),
      Position(x = 5, y = 5, z = 10),
      Position(x = 2, y = -7, z = 3),
      Position(x = 9, y = -8, z = -3))

    calculateTotalEndEnergy(startingPositions, numIterations = 100) shouldBe 1940
  }

  it should "produce correct solution for puzzle 1" in {
    val startingPositions = read(filePath)

    calculateTotalEndEnergy(startingPositions, numIterations = 1000) shouldBe 14809
  }

}
