package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day03 extends AoCDay(2021, 3) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    val lines = getLines

    @tailrec
    def findBinaryValue(remainingLines: List[String], bitCriteria: Char, index: Int = 0): String = {
      def getNextRemainingLines(currentRemainingLines: List[String], bitCriteria: Char, index: Int): List[String] =
        currentRemainingLines
          .map(_.charAt(index))
          .groupBy(c => c)
          .view
          .mapValues(_.length)
          .toList
          .sortBy(_._2) match {
          case (_, aQuantity) :: (_, bQuantity) :: Nil if aQuantity == bQuantity =>
            currentRemainingLines.filter(_.charAt(index) == bitCriteria)

          case (a, _) :: (_, _) :: Nil if bitCriteria == '0' => currentRemainingLines.filter(_.charAt(index) == a)

          case (_, _) :: (b, _) :: Nil if bitCriteria == '1' => currentRemainingLines.filter(_.charAt(index) == b)

        }

      remainingLines match {
        case Nil => throw new Exception("Unexpected empty list")

        case head :: Nil => head

        case multipleValues if index >= multipleValues.head.length => throw new IndexOutOfBoundsException

        case multipleValues =>
          findBinaryValue(
            getNextRemainingLines(multipleValues, bitCriteria, index),
            bitCriteria,
            index + 1
          )
      }
    }

    def part1(mostCommon: Boolean): Int = {
      Integer.parseInt(
        lines.head.indices.map { index =>
          lines
            .map(_.charAt(index))
            .groupBy(c => c)
            .view
            .mapValues(_.length)
            .toList
            .sortBy(_._2)
            .map(_._1) match {
            case _ :: max :: Nil if mostCommon => max
            case min :: _ :: Nil               => min
          }
        }.mkString,
        2
      )
    }

    lazy val (gammaRate, epsilonRate) = (part1(mostCommon = true), part1(mostCommon = false))
    println(s"Part 1: $gammaRate * $epsilonRate = ${gammaRate * epsilonRate}")

    lazy val oxygenGeneratorRating = Integer.parseInt(findBinaryValue(lines.toList, bitCriteria = '1'), 2)
    lazy val co2ScrubberRating = Integer.parseInt(findBinaryValue(lines.toList, bitCriteria = '0'), 2)
    println(s"Part 2: $oxygenGeneratorRating * $co2ScrubberRating = ${oxygenGeneratorRating * co2ScrubberRating}")
  }
}
