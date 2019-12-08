package com.jswarburton.adventofcode.day08

import com.jswarburton.adventofcode.day08.SpaceImageFormat._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SpaceImageFormatTest extends AnyFlatSpec with Matchers {
  private val filePath = "src/main/resources/day08/puzzle1and2-input.txt"

  behavior of "SpaceImageFormat"

  it should "split layers" in {
    val data = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2)

    splitLayers(data, width = 3, height = 2) shouldBe List(List(1, 2, 3, 4, 5, 6), List(7, 8, 9, 0, 1, 2))
  }

  it should "produce the correct result for puzzle 1" in {
    val data = read(filePath)
    val layers = splitLayers(data, width = 25, height = 6)
    val layerWithFewestZeroes = findLayerWithFewestX(layers, 0)

    numXByNumY(layerWithFewestZeroes, 1, 2) shouldBe 1965
  }

}
