package com.jswarburton.adventofcode.intcode

import com.jswarburton.adventofcode.intcode.IntCode.parseOpCodeAndModes
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntCodeTest extends AnyFlatSpec with Matchers {

  behavior of "IntCode"

  it should "parse Op Codes and Modes" in {
    parseOpCodeAndModes(1002) shouldBe(2, PositionMode, ImmediateMode, PositionMode)
    parseOpCodeAndModes(10104) shouldBe(4, ImmediateMode, PositionMode, ImmediateMode)
  }
}
