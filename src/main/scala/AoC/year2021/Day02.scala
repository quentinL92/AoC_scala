package AoC.year2021

import AoC.AoCDay

import scala.util.matching.Regex

object Day02 extends AoCDay(2021, 2) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {
    val Forward: Regex = """forward (\d+)""".r
    val Down: Regex = """down (\d+)""".r
    val Up: Regex = """up (\d+)""".r

    lazy val partialFNPart1: ((Int, Int), String) => (Int, Int) = {
      case ((hor, depth), Forward(distance)) => (hor + distance.toInt) -> depth
      case ((hor, depth), Down(distance)) => hor -> (depth + distance.toInt)
      case ((hor, depth), Up(distance)) => hor -> (depth - distance.toInt)
    }

    lazy val partialFNPart2: ((Int, Int, Int), String) => (Int, Int, Int) = {
      case ((hor, depth, aim), Forward(distance)) => (hor + distance.toInt, depth + (aim * distance.toInt), aim)
      case ((hor, depth, aim), Down(distance)) => (hor, depth, aim  + distance.toInt)
      case ((hor, depth, aim), Up(distance)) => (hor, depth, aim  - distance.toInt)
    }

    val (finalHor, finalDepth, _) = getLines.foldLeft((0, 0, 0))(partialFNPart2)
    println(finalHor * finalDepth)
  }
}
