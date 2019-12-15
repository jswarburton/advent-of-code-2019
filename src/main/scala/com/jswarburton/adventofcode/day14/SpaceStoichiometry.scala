package com.jswarburton.adventofcode.day14

import scala.io.Source

object SpaceStoichiometry {
  def read(filePath: String): Map[String, (Int, List[(String, Int)])] = {
    Source.fromFile(filePath).getLines.toList.map {
      a =>
        a.split(" => ") match {
          case Array(a1, a2) => {
            val resultSplits = a2.trim.split(" ")
            val resultChemical = resultSplits(1)
            val numResults = resultSplits(0).toInt

            val reactantsSplit = a1.split(", ").toList
            val reactantsList = reactantsSplit.map {
              b => (b.split(" ")(1), b.split(" ")(0).toInt)
            }

            resultChemical -> (numResults, reactantsList)
          }
        }
    }.toMap
  }

  def amountOfOreNeededForFuel(reactions: Map[String, (Int, List[(String, Int)])]): Long = {

    def rec(chem: String,
            amount: Long = 1,
            excess: Map[String, Long] = Map().withDefaultValue(0)): (Long, Map[String, Long]) =
      if (chem == "ORE") (amount, excess)
      else {
        val amountWithoutExcess = math.max(0L, amount - excess(chem))
        val amountFromExcess = amount - amountWithoutExcess
        val excessWithoutAmount = excess + (chem -> (excess(chem) - amountFromExcess))

        val (outputAmount, chems) = reactions(chem)

        val (reactionRepeat, outputExcess) = (amountWithoutExcess / outputAmount, amountWithoutExcess % outputAmount) match {
          case (q, 0) => (q, 0L)
          case (q, rem) => (q + 1, outputAmount - rem)
        }

        val (ore, inputExcess) = chems.foldLeft((0L, excessWithoutAmount))({
          case ((oreAmount, excess), (inputChemical, inputAmount)) =>
            val (inputOre, inputExcess) = rec(inputChemical, reactionRepeat * inputAmount, excess)
            (oreAmount + inputOre, inputExcess)
        })

        (ore, inputExcess + (chem -> (inputExcess(chem) + outputExcess)))
      }

    rec("FUEL")._1
  }

}
