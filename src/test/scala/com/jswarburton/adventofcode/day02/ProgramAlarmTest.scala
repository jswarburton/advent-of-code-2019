package com.jswarburton.adventofcode.day02

import com.jswarburton.adventofcode.day02.ProgramAlarm.puzzle1
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ProgramAlarmTest extends AnyFlatSpec with Matchers {

  behavior of "Program Alarm"

  it should "produce correct result for puzzle 1" in {
    val filePath = "src/main/resources/day02/puzzle1-input.txt"

    puzzle1(filePath) shouldBe 3895705
  }

  it should "produce the correct output" in {
    ProgramAlarm.run(List(1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50)) shouldBe List(3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50)
    ProgramAlarm.run(List(1, 0, 0, 0, 99)) shouldBe List(2, 0, 0, 0, 99)
    ProgramAlarm.run(List(2, 3, 0, 3, 99)) shouldBe List(2, 3, 0, 6, 99)
    ProgramAlarm.run(List(2, 4, 4, 5, 99, 0)) shouldBe List(2, 4, 4, 5, 99, 9801)
    ProgramAlarm.run(List(1, 1, 1, 4, 99, 5, 6, 0, 99)) shouldBe List(30, 1, 1, 4, 2, 5, 6, 0, 99)
  }


}
