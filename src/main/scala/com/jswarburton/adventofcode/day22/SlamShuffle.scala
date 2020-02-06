package com.jswarburton.adventofcode.day22

import scala.io.Source

object SlamShuffle {
  type Deck = Vector[Int]

  def read(filePath: String): List[Operation] = Source.fromFile(filePath).getLines.toList.map(parseOperation)

  def parseOperation(str: String): Operation =
    if (str.contains("deal into new stack")) DealIntoNewStack
    else if (str.startsWith("deal with increment")) DealWithIncrement(str.replace("deal with increment ", "").toInt)
    else Cut(str.replace("cut ", "").toInt)

  def performOperations(deck: Deck, ops: List[Operation]): Deck =
    ops.foldLeft(deck) {
      (currentDeck, op) => op.performOp(currentDeck)
    }

  def generateFactoryOrderDeck(n: Int): Deck = (0 until n).toVector
}



