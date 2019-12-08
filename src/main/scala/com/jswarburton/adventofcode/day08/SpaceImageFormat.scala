package com.jswarburton.adventofcode.day08

import scala.io.Source

object SpaceImageFormat {
  def splitLayers(data: List[Int], width: Int, height: Int): List[List[Int]] = data.grouped(width * height).toList

  def findLayerWithFewestX(layers: List[List[Int]], x: Int): List[Int] =
    layers.foldLeft((Integer.MAX_VALUE, List[Int]())) {
      case ((i, oldLayer), layer) =>
        val numX = layer.count(_ == x)
        if (numX < i) (numX, layer) else (i, oldLayer)
    }._2

  def numXByNumY(layer: List[Int], x: Int, y: Int): Int = layer.count(_ == x) * layer.count(_ == y)

  def read(filePath: String): List[Int] = Source.fromFile(filePath).getLines.toList.head.grouped(1).toList.map(_.toInt)
}
