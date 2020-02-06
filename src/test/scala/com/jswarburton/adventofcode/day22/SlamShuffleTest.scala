package com.jswarburton.adventofcode.day22

import com.jswarburton.adventofcode.day22.SlamShuffle.{generateFactoryOrderDeck, performOperations, read}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SlamShuffleTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day22/puzzle1and2-input.txt"

  private def testCasePath(testNumber: Int) = s"src/test/resources/day22/10card-test$testNumber.txt"

  behavior of "SlamShuffle"

  it should "produce the correct solution for all test cases" in {
    val deck = generateFactoryOrderDeck(10)

    performOperations(deck, read(testCasePath(1))) shouldBe List(0, 3, 6, 9, 2, 5, 8, 1, 4, 7)
    performOperations(deck, read(testCasePath(2))) shouldBe List(3, 0, 7, 4, 1, 8, 5, 2, 9, 6)
    performOperations(deck, read(testCasePath(3))) shouldBe List(6, 3, 0, 7, 4, 1, 8, 5, 2, 9)
    performOperations(deck, read(testCasePath(4))) shouldBe List(9, 2, 5, 8, 1, 4, 7, 0, 3, 6)
  }

  it should "produce the correct solution for puzzle 1" in {
    val operations = read(filePath)
    val startingDeck = generateFactoryOrderDeck(10007)

    val finalDeck = performOperations(startingDeck, operations)

    finalDeck.indexOf(2019) shouldBe 1510
  }

}
