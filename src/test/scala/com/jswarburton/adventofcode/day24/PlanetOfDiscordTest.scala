package com.jswarburton.adventofcode.day24

import com.jswarburton.adventofcode.day24.PlanetOfDiscord.{calculateBiodiversity, read, runUntilPreviouslySeenState}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PlanetOfDiscordTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day24/puzzle1and2-input.txt"

  behavior of "Planet of Discord"

  it should "produce the correct solution for puzzle 1" in {
    val grid = read(filePath)

    val finalState = runUntilPreviouslySeenState(grid)

    val bioDiversity = calculateBiodiversity(finalState)

    bioDiversity shouldBe 1151290
  }
}
