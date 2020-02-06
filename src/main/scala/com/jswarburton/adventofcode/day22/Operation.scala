package com.jswarburton.adventofcode.day22

import com.jswarburton.adventofcode.day22.SlamShuffle.Deck

import scala.annotation.tailrec

sealed trait Operation {
  def performOp(deck: Deck): Deck
}

case object DealIntoNewStack extends Operation {
  override def performOp(deck: Deck): Deck = deck.reverse
}

case class DealWithIncrement(increment: Int) extends Operation {
  override def performOp(deck: Deck): Deck = {

    @tailrec
    def rec(remainingDeck: Deck,
            index: Int = 0,
            mapping: Map[Int, Int] = Map()): Map[Int, Int] = {
      if (remainingDeck.isEmpty) mapping
      else {
        val newMapping = mapping + (index -> remainingDeck.head)
        val newIndex = (index + increment) % deck.length

        rec(remainingDeck.tail, newIndex, newMapping)
      }
    }

    val finalMapping = rec(deck)

    (0 until deck.length).map(finalMapping).toVector
  }
}

case class Cut(n: Int) extends Operation {
  override def performOp(deck: Deck): Deck = {
    val numToCut = if (n < 0) deck.size + n else n
    deck.takeRight(deck.size - numToCut) ++ deck.take(numToCut)
  }
}