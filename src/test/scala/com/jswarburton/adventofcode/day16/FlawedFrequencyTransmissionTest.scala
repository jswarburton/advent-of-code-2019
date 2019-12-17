package com.jswarburton.adventofcode.day16

import com.jswarburton.adventofcode.day16.FlawedFrequencyTransmission.fft
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlawedFrequencyTransmissionTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day16/puzzle1and2-input.txt"

  behavior of "Flawed Frequency Transmission"

  it should "produce the correct result for debug case" in {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8)
    fft(input, numPhases = 1) shouldBe 48226158
    fft(input, numPhases = 2) shouldBe 34040438
    fft(input, numPhases = 3) shouldBe 3415518
    fft(input, numPhases = 4) shouldBe 1029498
  }

  it should "produce the correct result for test case 1" in {
    val input = List(8, 0, 8, 7, 1, 2, 2, 4, 5, 8, 5, 9, 1, 4, 5, 4, 6, 6, 1, 9, 0, 8, 3, 2, 1, 8, 6, 4, 5, 5, 9, 5)
    fft(input, numPhases = 100) shouldBe 24176176
  }

  it should "produce the correct result for test case 2" in {
    val input = List(1, 9, 6, 1, 7, 8, 0, 4, 2, 0, 7, 2, 0, 2, 2, 0, 9, 1, 4, 4, 9, 1, 6, 0, 4, 4, 1, 8, 9, 9, 1, 7)
    fft(input, numPhases = 100) shouldBe 73745418
  }

  it should "produce the correct result for test case 3" in {
    val input = List(6, 9, 3, 1, 7, 1, 6, 3, 4, 9, 2, 9, 4, 8, 6, 0, 6, 3, 3, 5, 9, 9, 5, 9, 2, 4, 3, 1, 9, 8, 7, 3)
    fft(input, numPhases = 100) shouldBe 52432133
  }

  it should "produce correct solution for puzzle 1" in {
    val input = FlawedFrequencyTransmission.read(filePath)
    fft(input, numPhases = 100) shouldBe 30550349
  }

}
