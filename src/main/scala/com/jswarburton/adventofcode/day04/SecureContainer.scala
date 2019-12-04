package com.jswarburton.adventofcode.day04

import scala.annotation.tailrec

object SecureContainer {

  def puzzle1(start: Int, end: Int): Int = {
    @tailrec
    def helper(curr: Int, numSoFar: Int = 0): Int = {
      if (curr > end) numSoFar
      else if (digitsAreIncreasing(curr) && twoAdjacentDigitsAreSame(curr)) helper(curr + 1, numSoFar + 1)
      else helper(curr + 1, numSoFar)
    }

    helper(start)
  }

  def digitsAreIncreasing(i: Int): Boolean = {
    val listified = i.toString.toList.map(_.toInt)
    listified.sliding(2).forall { case Seq(x, y) => y >= x }
  }

  def twoAdjacentDigitsAreSame(i: Int): Boolean = {
    val listified = i.toString.toList.map(_.toInt)
    listified.sliding(2).exists { case Seq(x, y) => x == y }
  }

}
