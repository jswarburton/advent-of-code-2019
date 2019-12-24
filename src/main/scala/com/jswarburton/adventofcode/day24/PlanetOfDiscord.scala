package com.jswarburton.adventofcode.day24

import scala.annotation.tailrec
import scala.io.Source

case class Coord(x: Int, y: Int) {
  lazy val adjacent: List[Coord] = List(copy(x = x + 1), copy(x = x - 1), copy(y = y + 1), copy(y = y - 1))
}

object PlanetOfDiscord {

  type Grid = Map[Coord, Boolean]

  def read(filePath: String): Grid = Source.fromFile(filePath).getLines.toList
    .zipWithIndex
    .flatMap {
      case (str, y) => str.toList.zipWithIndex.map {
        case (char, x) => (Coord(x, y), char == '#')
      }
    }.toMap

  def runUntilPreviouslySeenState(grid: Grid): Grid = {
    @tailrec
    def rec(currentGrid: Grid, previouslySeen: Set[Grid] = Set()): Grid =
      if (previouslySeen.contains(currentGrid)) currentGrid
      else rec(runIter(currentGrid), previouslySeen + currentGrid)

    rec(grid)
  }

  def runIter(grid: Grid): Grid = grid.keySet.map {
    coord => if (grid(coord)) moveBug(grid, coord) else moveEmpty(grid, coord)
  }.toMap

  /**
   * A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it.
   */
  private def moveBug(grid: Grid, coord: Coord): (Coord, Boolean) = (coord, numBugsAdjacent(grid, coord) == 1)

  private def numBugsAdjacent(grid: Grid, coord: Coord): Int = coord.adjacent.map(grid.withDefaultValue(false)(_)).count(_ == true)

  /**
   * An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
   */
  private def moveEmpty(grid: Grid, coord: Coord): (Coord, Boolean) = {
    val n = numBugsAdjacent(grid, coord)
    (coord, n == 1 || n == 2)
  }

  def calculateBiodiversity(grid: Grid): Int = {
    val bugCoords = grid.filter(_._2).keySet

    bugCoords.map(c => Math.pow(2, 5 * c.y + c.x).toInt).sum
  }
}
