package com.jswarburton.adventofcode.day01

import com.jswarburton.adventofcode.day01.TheTyrannyOfTheRocketEquation._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TheTyrannyOfTheRocketEquationTest extends AnyFlatSpec with Matchers {

  behavior of "The Tyranny of the Rocket Equation"

  it should "produce the correct fuel requirement" in {
    val masses = List(12, 14, 1969, 100756)

    val expectedFuelRequireds = List(2, 2, 654, 33583)

    val actualFuelRequireds = masses.map(simpleFuelRequired)

    actualFuelRequireds shouldBe expectedFuelRequireds
  }

  it should "produce the correct solution for puzzle 1" in {
    val masses = read("src/main/resources/day01/puzzle1and2-input.txt")
    puzzle1(masses) shouldBe 3317668
  }

  it should "produce the correct recursive fuel required" in {
    recursiveFuelRequired(1969) shouldBe 966
    recursiveFuelRequired(100756) shouldBe 50346
  }

  it should "print out the puzzle 2 solution" in {
    val masses = read("src/main/resources/day01/puzzle1and2-input.txt")
    puzzle2(masses) shouldBe 4973628
  }

}
