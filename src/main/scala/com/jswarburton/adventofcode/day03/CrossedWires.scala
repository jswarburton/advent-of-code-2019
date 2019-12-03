package com.jswarburton.adventofcode.day03

import scala.annotation.tailrec
import scala.io.Source

case class DirectionAndDistance(direction: Char, distance: Int)

case class Point(x: Int, y: Int) {
  def left(distance: Int): Point = copy(x = x - distance)

  def right(distance: Int): Point = copy(x = x + distance)

  def up(distance: Int): Point = copy(y = y + distance)

  def down(distance: Int): Point = copy(y = y - distance)
}

object CrossedWires {

  def puzzle1(filePath: String): Int = {
    val (wire1Path, wire2Path) = read(filePath)
    manhattanDistanceToNearestIntersection(wire1Path, wire2Path)
  }

  def read(filePath: String): (List[DirectionAndDistance], List[DirectionAndDistance]) = {
    val lines = Source.fromFile(filePath).getLines.toList.map(line =>
      line.split(",")
        .map { dir => DirectionAndDistance(dir.head, dir.tail.toInt) }
        .toList
    )

    (lines.head, lines(1))
  }

  def manhattanDistanceToNearestIntersection(wire1Path: List[DirectionAndDistance],
                                             wire2Path: List[DirectionAndDistance]): Int = {
    @tailrec
    def genSeenPoints(remainingDirections: List[DirectionAndDistance],
                      currentPoint: Point = Point(0, 0),
                      seenPoints: Set[Point] = Set()): Set[Point] = {

      remainingDirections match {
        case Nil => seenPoints
        case dirs => {
          val nextDir = dirs.head

          val distance = nextDir.distance

          val (newSeenPoints, newCurrentPoint) =
            nextDir.direction match {
              case 'U' => ((1 to distance).toSet.map(currentPoint.up), currentPoint.up(distance))

              case 'D' => ((1 to distance).toSet.map(currentPoint.down), currentPoint.down(distance))

              case 'L' => ((1 to distance).toSet.map(currentPoint.left), currentPoint.left(distance))

              case 'R' => ((1 to distance).toSet.map(currentPoint.right), currentPoint.right(distance))
            }

          genSeenPoints(remainingDirections.tail, newCurrentPoint, seenPoints ++ newSeenPoints)
        }
      }
    }

    val seenPoints1 = genSeenPoints(wire1Path)
    val seenPoints2 = genSeenPoints(wire2Path)

    val intersections = seenPoints1.intersect(seenPoints2)

    val manhattanDistances = intersections.toList.map { case Point(x, y) => Math.abs(x) + Math.abs(y) }

    manhattanDistances.min
  }

}
