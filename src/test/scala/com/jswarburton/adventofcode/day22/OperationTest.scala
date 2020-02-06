package com.jswarburton.adventofcode.day22

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OperationTest extends AnyFlatSpec with Matchers {
  behavior of "SlamShuffle"

  it should "cut" in {
    val deck = Vector(1, 2, 3, 4, 5, 6)

    Cut(0).performOp(deck) shouldBe List(1, 2, 3, 4, 5, 6)
    Cut(2).performOp(deck) shouldBe List(3, 4, 5, 6, 1, 2)
    Cut(4).performOp(deck) shouldBe List(5, 6, 1, 2, 3, 4)
  }

  it should "cut negative value" in {
    val deck = Vector(1, 2, 3, 4, 5, 6)

    Cut(-2).performOp(deck) shouldBe Vector(5, 6, 1, 2, 3, 4)
    Cut(-4).performOp(deck) shouldBe Vector(3, 4, 5, 6, 1, 2)
  }

  it should "deal into new stack" in {
    val deck = Vector(1, 2, 3, 4, 5, 6)

    DealIntoNewStack.performOp(deck) shouldBe Vector(6, 5, 4, 3, 2, 1)
  }

  it should "deal with increment" in {
    val deck = SlamShuffle.generateFactoryOrderDeck(10)

    DealWithIncrement(3).performOp(deck) shouldBe Vector(0, 7, 4, 1, 8, 5, 2, 9, 6, 3)
  }

}
