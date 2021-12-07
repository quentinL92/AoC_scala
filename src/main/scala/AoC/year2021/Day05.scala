package AoC.year2021

import AoC.AoCDay

import scala.util.matching.Regex

object Day05 extends AoCDay(2021, 5) {
  override def resolveDay(): Unit = {

    val lines = getLines()
    val LineRegex: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r

    def part1(): Unit = {
      case class Line(start: (Int, Int), end: (Int, Int))

      val resultPart1 = lines
        .collect {
          case LineRegex(x1, y1, x2, y2) if x1 == x2 || y1 == y2 => Line(x1.toInt -> y1.toInt, x2.toInt -> y2.toInt)
        }
        .foldLeft(Map.empty[(Int, Int), Int]) {
          case (acc, Line((x1, y1), (x2, y2))) =>
            val xOrdered = List(x1, x2).sorted
            val yOrdered = List(y1, y2).sorted
            (
              for {
                x <- xOrdered.head to xOrdered.last
                y <- yOrdered.head to yOrdered.last
              } yield x -> y
            ).foldLeft(acc) {
              case (innerAcc, key) => innerAcc.updated(key, innerAcc.getOrElse(key, 0) + 1)
            }
        }
        .count(_._2 >= 2)

      println(s"Part1 = $resultPart1")
    }

    def part2(): Unit = {
      case class OrientedLine(start: (Int, Int), end: (Int, Int), step: (Int, Int))
      object OrientedLine {
        def apply(x1: Int, y1: Int, x2: Int, y2: Int): OrientedLine = {
          val xDiff = x2 - x1
          val yDiff = y2 - y1
          val xDiv = if (xDiff == 0) 1 else Math.abs(xDiff)
          val yDiv = if (yDiff == 0) 1 else Math.abs(yDiff)
          OrientedLine(x1 -> y1, x2 -> y2, (xDiff / xDiv) -> (yDiff / yDiv))
        }
      }

      val resultPart2 = lines
        .collect {
          case LineRegex(x1, y1, x2, y2) =>
            OrientedLine(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
        }
        .foldLeft(Map.empty[(Int, Int), Int]) {
          case (acc, OrientedLine((x1, y1), (x2, y2), (stepX, stepY))) =>
            val xs = if (stepX == 0) x1 to x2 else x1 to (x2, stepX)
            val ys = if (stepY == 0) y1 to y2 else y1 to (y2, stepY)
            xs.zipAll(ys, xs.head, ys.head).foldLeft(acc) {
              case (innerAcc, key) => innerAcc.updated(key, innerAcc.getOrElse(key, 0) + 1)
            }
        }
        .count(_._2 >= 2)

      println(s"Part2 = $resultPart2")
    }

    part1()
    part2()
  }
}
