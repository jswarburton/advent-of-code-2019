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
}
