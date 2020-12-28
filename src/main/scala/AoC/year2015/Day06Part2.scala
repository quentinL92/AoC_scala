package AoC.year2015

import AoC.AoCDay

import scala.util.matching.Regex

object Day06Part2 extends AoCDay(2015, 6) {

  val InstructionLine: Regex = """(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)""".r

  def coor(s: String) = {
    val Array(a, b) = s.split(",")
    a.toInt -> b.toInt
  }

  val grid: Array[Array[Int]] = Array.fill(1000, 1000)(0)
  for {
    InstructionLine(instruction, fromx, fromy, tox, toy) <- getLines()
    x <- fromx.toInt to tox.toInt
    y <- fromy.toInt to toy.toInt
  } instruction match {
    case "turn on" => grid(x)(y) += 1
    case "turn off" if grid(x)(y) - 1 < 0 => grid(x)(y) = 0
    case "turn off" => grid(x)(y) = grid(x)(y) - 1
    case "toggle" => grid(x)(y) += 2
  }

  val res2 = grid.flatten.sum
  println(res2)
}
