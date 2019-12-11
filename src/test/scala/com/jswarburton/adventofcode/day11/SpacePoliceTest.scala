package com.jswarburton.adventofcode.day11

import com.jswarburton.adventofcode.day11.SpacePolice._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpacePoliceTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day11/puzzle1and2-input.txt"

  behavior of "Space Police"

  it should "run puzzle 1" in {
    numSquaresPainted(read(filePath)) shouldBe 2219
  }

}
