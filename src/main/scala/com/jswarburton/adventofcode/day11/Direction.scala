package com.jswarburton.adventofcode.day11

sealed trait Direction {
  def clockwise: Direction

  def antiClockwise: Direction
}

case object Up extends Direction {
  override def clockwise: Direction = Right

  override def antiClockwise: Direction = Left
}

case object Down extends Direction {
  override def clockwise: Direction = Left

  override def antiClockwise: Direction = Right
}

case object Right extends Direction {
  override def clockwise: Direction = Down

  override def antiClockwise: Direction = Up
}

case object Left extends Direction {
  override def clockwise: Direction = Up

  override def antiClockwise: Direction = Down
}