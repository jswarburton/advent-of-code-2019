package com.jswarburton.adventofcode.day10

import scala.io.Source

case class Coordinate(x: Int, y: Int) {
  def angleTo(other: Coordinate): Double = Math.atan2(other.x - x, other.y - y)
}

object MonitoringStation {
  def read(filePath: String): Map[Coordinate, Boolean] =
    Source.fromFile(filePath).getLines.toList.zipWithIndex.flatMap {
      case (chars, y) =>
        chars.toList.zipWithIndex.map {
          case ('.', x) => Coordinate(x, y) -> false
          case ('#', x) => Coordinate(x, y) -> true
        }
    }.toMap

  def findBestMonitoringStation(asteroidMap: Map[Coordinate, Boolean]): (Coordinate, Int) = {
    val asteroids = asteroidMap
      .filter(_._2)
      .keySet

    asteroids.foldLeft((Coordinate(0, 0), Integer.MIN_VALUE)) {
      case ((bestCoord, maxAsteroids), newCoord) =>
        val numFromNewCoord = findNumAsteroidsFromCoord(asteroids, newCoord)
        if (numFromNewCoord > maxAsteroids) (newCoord, numFromNewCoord)
        else (bestCoord, maxAsteroids)
    }
  }

  def findNumAsteroidsFromCoord(asteroids: Set[Coordinate], coordinate: Coordinate): Int = {
    val otherAsteroids = asteroids - coordinate

    otherAsteroids.groupBy(coordinate.angleTo)
      .size
  }
}