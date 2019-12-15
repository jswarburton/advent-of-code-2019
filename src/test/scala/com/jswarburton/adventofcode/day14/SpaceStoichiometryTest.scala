package com.jswarburton.adventofcode.day14

import com.jswarburton.adventofcode.day14.SpaceStoichiometry.{amountOfOreNeededForFuel, read}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpaceStoichiometryTest extends AnyFlatSpec with Matchers {

  behavior of "Space Stoichiometry"

  private val testResourcesDir = "src/test/resources/day14/"
  private val filePath = "src/main/resources/day14/puzzle1and2-input.txt"

  it should "produce correct amount of ore required for 1 fuel for test case 1" in {
    val reactions = read(testResourcesDir + "test-case-01.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 31
  }

  it should "produce correct amount of ore required for 1 fuel for test case 2" in {
    val reactions = read(testResourcesDir + "test-case-02.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 165
  }

  it should "produce correct result for puzzle 1" in {
    val reactions = read(filePath)

    amountOfOreNeededForFuel(reactions) shouldBe 522031
  }

}
