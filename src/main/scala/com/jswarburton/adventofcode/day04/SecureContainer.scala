package com.jswarburton.adventofcode.day04

import scala.annotation.tailrec

object SecureContainer {

  def puzzle1(start: Int, end: Int): Int = {
    @tailrec
    def rec(curr: Int, numSoFar: Int = 0): Int = {
      if (curr > end) numSoFar
      else if (digitsAreIncreasing(curr) && twoAdjacentDigitsAreSame(curr)) rec(curr + 1, numSoFar + 1)
      else rec(curr + 1, numSoFar)
    }

    rec(start)
  }

  def twoAdjacentDigitsAreSame(i: Int): Boolean = {
    val listified = i.toString.toList.map(_.toInt)

    @tailrec
    def rec(previousDigit: Int, remainingDigits: List[Int]): Boolean = {
      remainingDigits match {
        case Nil => false
        case x :: _ if x == previousDigit => true
        case x :: xs => rec(x, xs)
      }
    }

    rec(listified.head, listified.tail)
  }

  def puzzle2(start: Int, end: Int): Int = {
    @tailrec
    def rec(curr: Int, numSoFar: Int = 0): Int = {
      if (curr > end) numSoFar
      else if (digitsAreIncreasing(curr) && onlyTwoAdjacentDigitsAreSame(curr)) rec(curr + 1, numSoFar + 1)
      else rec(curr + 1, numSoFar)
    }

    rec(start)
  }

  def digitsAreIncreasing(i: Int): Boolean = {
    val listified = i.toString.toList.map(_.toInt)
    @tailrec
    def rec(previousDigit: Int, remainingDigits: List[Int]): Boolean = {
      remainingDigits match {
        case Nil => true
        case x :: _ if x < previousDigit => false
        case x :: xs => rec(x, xs)
      }
    }

    rec(listified.head, listified.tail)
  }

  def onlyTwoAdjacentDigitsAreSame(i: Int): Boolean = {
    val listified = i.toString.toList.map(_.toInt)

    @tailrec
    def rec(previousDigit: Int, remainingDigits: List[Int], numConsecutive: Int = 1): Boolean = {
      remainingDigits match {
        case Nil => numConsecutive == 2
        case x :: xs =>
          if (x == previousDigit) rec(x, xs, numConsecutive + 1)
          else if (numConsecutive == 2) true
          else rec(x, xs)
      }
    }

    rec(listified.head, listified.tail)
  }

}
