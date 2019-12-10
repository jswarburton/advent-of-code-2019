package com.jswarburton.adventofcode.day10

import com.jswarburton.adventofcode.day10.MonitoringStation._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MonitoringStationTest extends AnyFlatSpec with Matchers {
  private val testResourcesDir = "src/test/resources/day10/"
  private val filePath = "src/main/resources/day10/puzzle1and2-input.txt"

  behavior of "Monitoring Station"

  it should "produce correct number of asteroids detected for test case 1" in {
    val asteroidMap = read(testResourcesDir + "test-case-01.txt")
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(5, 8), 33)
  }

  it should "produce correct number of asteroids detected for test case 2" in {
    val asteroidMap = read(testResourcesDir + "test-case-02.txt")
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(1, 2), 35)
  }

  it should "produce correct number of asteroids detected for test case 3" in {
    val asteroidMap = read(testResourcesDir + "test-case-03.txt")
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(6, 3), 41)
  }

  it should "produce correct number of asteroids detected for test case 4" in {
    val asteroidMap = read(testResourcesDir + "test-case-04.txt")
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(11, 13), 210)
  }

  it should "produce the correct result for puzzle 1" in {
    val asteroidMap = read(filePath)
    val (coordinate, numAsteroidsDetected) = findBestMonitoringStation(asteroidMap)

    (coordinate, numAsteroidsDetected) shouldBe(Coordinate(11, 11), 221)
  }
}
