package com.jswarburton.adventofcode.day06

import scala.annotation.tailrec
import scala.io.Source

case class Orbit(orbiter: String, orbitee: String)

object UniversalOrbitMap {
  def puzzle1(filePath: String): Int = totalNumOrbits(read(filePath))

  def totalNumOrbits(orbits: List[Orbit]): Int = {
    val orbiterToOrbitee = orbits.map(orbit => orbit.orbiter -> orbit.orbitee).toMap

    val orbiters = orbits.map(_.orbiter).toSet
    val orbitees = orbits.map(_.orbitee).toSet

    val centroids = orbitees.filter(!orbiters.contains(_))

    var memo: Map[String, Int] = Map()

    def findNumOrbits(orbiter: String): Int = {
      if (centroids.contains(orbiter)) 0
      else if (memo.contains(orbiter)) memo(orbiter)
      else {
        val res = 1 + findNumOrbits(orbiterToOrbitee(orbiter))
        memo = memo + (orbiter -> res)
        res
      }
    }

    @tailrec
    def findTotalNumOrbits(remainingOrbiters: Set[String],
                           count: Int = 0): Int = {
      if (remainingOrbiters.isEmpty) count
      else {
        val newOrbiter = remainingOrbiters.head
        val numOrbits = findNumOrbits(newOrbiter)
        findTotalNumOrbits(remainingOrbiters.tail, count + numOrbits)
      }
    }

    findTotalNumOrbits(orbiters)
  }

  def read(filePath: String): List[Orbit] =
    Source.fromFile(filePath).getLines.toList
      .map(line => line.split("\\)"))
      .map(splits => Orbit(splits.tail.head, splits.head))

  def puzzle2(filePath: String): Int = minOrbitalTransfersToSanta(read(filePath))

  def minOrbitalTransfersToSanta(orbits: List[Orbit]): Int = {
    val you = "YOU"
    val santa = "SAN"
    val centroid = "COM"

    val orbiterToOrbitee = orbits.map(orbit => orbit.orbiter -> orbit.orbitee).toMap

    @tailrec
    def pathToCentroid(orbiter: String, path: List[String] = List()): List[String] =
      if (orbiter == centroid) path
      else pathToCentroid(orbiterToOrbitee(orbiter), orbiter :: path)

    val youPathToCentroid = pathToCentroid(you)
    val santaPathToCentroid = pathToCentroid(santa)

    def findNumNonIntersections(path1: List[String], path2: List[String]): Int = {
      @tailrec
      def findDivergence(rem1: List[String], rem2: List[String]): (List[String], List[String]) =
        (rem1, rem2) match {
          case (Nil, Nil) => (Nil, Nil)
          case (Nil, something) => (Nil, something)
          case (something, Nil) => (something, Nil)
          case (head1 :: tail1, head2 :: tail2) if head1 == head2 => findDivergence(tail1, tail2)
          case (some1, some2) => (some1, some2)
        }

      val (div1, div2) = findDivergence(path1, path2)

      (div1.size - 1) + (div2.size - 1)
    }

    findNumNonIntersections(youPathToCentroid, santaPathToCentroid)
  }
}
