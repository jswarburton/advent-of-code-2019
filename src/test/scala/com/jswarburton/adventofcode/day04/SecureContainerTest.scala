package com.jswarburton.adventofcode.day04

import com.jswarburton.adventofcode.day04.SecureContainer.{digitsAreIncreasing, puzzle1, twoAdjacentDigitsAreSame}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SecureContainerTest extends AnyFlatSpec with Matchers {

  behavior of "SecureContainer"

  it should "check whether digits in int are always increasing" in {
    digitsAreIncreasing(111111) shouldBe true
    digitsAreIncreasing(12345) shouldBe true
    digitsAreIncreasing(223450) shouldBe false
  }

  it should "check whether two adjacent digits in int are the same" in {
    twoAdjacentDigitsAreSame(111111) shouldBe true
    twoAdjacentDigitsAreSame(12345) shouldBe false
    twoAdjacentDigitsAreSame(223450) shouldBe true
    twoAdjacentDigitsAreSame(122250) shouldBe true
  }

  it should "produce the correct answer for puzzle 1" in {
    puzzle1(152085, 670283) shouldBe 1764
  }

}
