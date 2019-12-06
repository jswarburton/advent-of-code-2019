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

    @tailrec
    def findNumOrbits(orbiter: String, count: Int = 0): Int =
      if (centroids.contains(orbiter)) count
      else findNumOrbits(orbiterToOrbitee(orbiter), count + 1)

    orbiters.toList.map(findNumOrbits(_)).sum
  }

  def read(filePath: String): List[Orbit] =
    Source.fromFile(filePath).getLines.toList
      .map(line => line.split("\\)"))
      .map(splits => Orbit(splits.tail.head, splits.head))
}
