package com.jswarburton.adventofcode.day06

import com.jswarburton.adventofcode.day06.UniversalOrbitMap._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UniversalOrbitMapTest extends AnyFlatSpec with Matchers {

  private val filePath = "src/main/resources/day06/puzzle1and2-input.txt"

  behavior of "UniversalOrbitMap"

  it should "produce correct result for puzzle 1" in {
    puzzle1(filePath) shouldBe 333679
  }

  it should "produce correct total number of orbits" in {
    val orbits = List(
      Orbit("B", "COM"),
      Orbit("C","B"),
      Orbit("D","C"),
      Orbit("E","D"),
      Orbit("F","E"),
      Orbit("G","B"),
      Orbit("H","G"),
      Orbit("I","D"),
      Orbit("J","E"),
      Orbit("K","J"),
      Orbit("L","K"))

    totalNumOrbits(orbits) shouldBe 42
  }

}
