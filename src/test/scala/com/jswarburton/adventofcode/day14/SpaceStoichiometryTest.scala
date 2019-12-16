package com.jswarburton.adventofcode.day14

import com.jswarburton.adventofcode.day14.SpaceStoichiometry.{amountOfOreNeededForFuel, read, maxAmountOfFuel}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpaceStoichiometryTest extends AnyFlatSpec with Matchers {

  behavior of "Space Stoichiometry"

  private val testResourcesDir = "src/test/resources/day14/"
  private val filePath = "src/main/resources/day14/puzzle1and2-input.txt"

  private val oneTrillion = 1_000_000_000_000L

  it should "produce correct amount of ore required for 1 fuel for test case 1" in {
    val reactions = read(testResourcesDir + "test-case-01.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 31
  }

  it should "produce correct amount of ore required for 1 fuel for test case 2" in {
    val reactions = read(testResourcesDir + "test-case-02.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 165
  }

  it should "produce correct amount of ore required for 1 fuel for test case 3" in {
    val reactions = read(testResourcesDir + "test-case-03.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 13312
  }

  it should "produce correct amount of ore required for 1 fuel for test case 4" in {
    val reactions = read(testResourcesDir + "test-case-04.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 180697
  }

  it should "produce correct amount of ore required for 1 fuel for test case 5" in {
    val reactions = read(testResourcesDir + "test-case-05.txt")

    amountOfOreNeededForFuel(reactions) shouldBe 2210736
  }

  it should "produce correct result for puzzle 1" in {
    val reactions = read(filePath)

    amountOfOreNeededForFuel(reactions) shouldBe 522031
  }

  it should "produce correct max amount of fuel  produced from 1 trillion ore for test case 3" in {
    val reactions = read(testResourcesDir + "test-case-03.txt")

    maxAmountOfFuel(reactions, oneTrillion) shouldBe 82892753
  }

  it should "produce correct max amount of fuel  produced from 1 trillion ore for test case 4" in {
    val reactions = read(testResourcesDir + "test-case-04.txt")

    maxAmountOfFuel(reactions, oneTrillion) shouldBe 5586022
  }

  it should "produce correct max amount of fuel  produced from 1 trillion ore for test case 5" in {
    val reactions = read(testResourcesDir + "test-case-05.txt")

    maxAmountOfFuel(reactions, oneTrillion) shouldBe 460664
  }

  it should "produce correct result for puzzle 2" in {
    val reactions = read(filePath)

    maxAmountOfFuel(reactions, oneTrillion) shouldBe 3566577
  }

}
