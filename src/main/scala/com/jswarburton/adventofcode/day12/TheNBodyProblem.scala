package com.jswarburton.adventofcode.day12

import scala.annotation.tailrec
import scala.io.Source

case class Position(x: Int, y: Int, z: Int)

case class Velocity(x: Int = 0, y: Int = 0, z: Int = 0)

case class PositionAndVelocity(pos: Position, vel: Velocity) {
  def totalEnergy: Int = kineticEnergy * potentialEnergy

  def kineticEnergy: Int = vel.x.abs + vel.y.abs + vel.z.abs

  def potentialEnergy: Int = pos.x.abs + pos.y.abs + pos.z.abs
}

object TheNBodyProblem {

  def read(filePath: String): List[Position] = {
    val pattern = """<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>""".r
    Source.fromFile(filePath).getLines.toList.map(_ match {
      case pattern(x, y, z) => Position(x.toInt, y.toInt, z.toInt)
    })
  }

  def calculateTotalEndEnergy(startingPositions: List[Position], numIterations: Int): Int = {

    val initialStates = startingPositions.map(PositionAndVelocity(_, Velocity()))

    val indexCombinations = (0 until startingPositions.size).combinations(2)
      .map(indices => (indices(0), indices(1))).toList

    @tailrec
    def rec(currentStates: Vector[PositionAndVelocity],
            iterations: Int = 0): Vector[PositionAndVelocity] = {
      if (iterations == numIterations) currentStates
      else {
        val gravityApplied = indexCombinations.foldLeft(currentStates)((states, indices) => {
          val (state1, state2) = (states(indices._1), states(indices._2))

          def applyGravity(targetBody: PositionAndVelocity, otherBody: PositionAndVelocity): PositionAndVelocity = {
            def newValue(positionToCoord: Position => Int, velToCoord: Velocity => Int): Int =
              if (positionToCoord(targetBody.pos) > positionToCoord(otherBody.pos)) velToCoord(targetBody.vel) - 1
              else if (positionToCoord(targetBody.pos) == positionToCoord(otherBody.pos)) velToCoord(targetBody.vel)
              else velToCoord(targetBody.vel) + 1

            PositionAndVelocity(targetBody.pos, Velocity(newValue(_.x, _.x), newValue(_.y, _.y), newValue(_.z, _.z)))
          }

          val newState1 = applyGravity(state1, state2)
          val newState2 = applyGravity(state2, state1)

          states
            .updated(indices._1, newState1)
            .updated(indices._2, newState2)
        })

        val velocityApplied = gravityApplied.map {
          case PositionAndVelocity(Position(pX, pY, pZ), Velocity(vX, vY, vZ)) =>
            PositionAndVelocity(Position(pX + vX, pY + vY, pZ + vZ), Velocity(vX, vY, vZ))
        }

        rec(velocityApplied, iterations + 1)
      }
    }

    val finalStates = rec(initialStates.toVector)

    finalStates.map(_.totalEnergy).sum
  }
}
