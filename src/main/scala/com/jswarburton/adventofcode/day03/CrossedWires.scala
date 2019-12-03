package com.jswarburton.adventofcode.day03

import scala.annotation.tailrec
import scala.io.Source

case class DirectionAndDistance(direction: Char, distance: Int)

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
                      currentPoint: (Int, Int) = (0, 0),
                      seenPoints: Set[(Int, Int)] = Set()): Set[(Int, Int)] = {

      remainingDirections match {
        case Nil => seenPoints
        case dirs => {
          val nextDir = dirs.head

          val (newSeenPoints, newCurrentPoint) =
            nextDir.direction match {
              case 'U' =>
                ((1 to nextDir.distance).foldLeft(Set[(Int, Int)]()) {
                  (a, b) => a + ((currentPoint._1, currentPoint._2 + b))
                }, (currentPoint._1, currentPoint._2 + nextDir.distance))

              case 'D' =>
                ((1 to nextDir.distance).foldLeft(Set[(Int, Int)]()) {
                  (a, b) => a + ((currentPoint._1, currentPoint._2 - b))
                }, (currentPoint._1, currentPoint._2 - nextDir.distance))

              case 'L' =>
                ((1 to nextDir.distance).foldLeft(Set[(Int, Int)]()) {
                  (a, b) => a + ((currentPoint._1 - b, currentPoint._2))
                }, (currentPoint._1 - nextDir.distance, currentPoint._2))

              case 'R' =>
                ((1 to nextDir.distance).foldLeft(Set[(Int, Int)]()) {
                  (a, b) => a + ((currentPoint._1 + b, currentPoint._2))
                }, (currentPoint._1 + nextDir.distance, currentPoint._2))

            }

          genSeenPoints(remainingDirections.tail, newCurrentPoint, seenPoints ++ newSeenPoints)
        }
      }
    }

    val seenPoints1 = genSeenPoints(wire1Path)
    val seenPoints2 = genSeenPoints(wire2Path)

    val intersections = seenPoints1.intersect(seenPoints2)

    val manhattanDistances = intersections.toList.map { case (x, y) => Math.abs(x) + Math.abs(y) }

    manhattanDistances.min
  }

}
