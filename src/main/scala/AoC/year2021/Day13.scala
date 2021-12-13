package AoC.year2021

import AoC.AoCDay

object Day13 extends AoCDay(2021, 13) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    lazy val lines = getLines

    val (pointsSeparated, foldInstructionsSeparated) = lines.filter(_.trim.nonEmpty).partition(_.contains(","))
    val points: Set[(Int, Int)] = pointsSeparated
      .map(_.trim.split(',').map(_.toInt) match {
        case Array(x, y) => x -> y
      })
      .toSet
    val foldInstructions: Vector[(Char, Int)] = foldInstructionsSeparated.map(_.trim.split('=') match {
      case Array(head, value) => (head.last, value.toInt)
    })

    def foldSheet(currentPoints: Set[(Int, Int)], foldDirection: Char, foldValue: Int): Set[(Int, Int)] = {
      val (pointsToFold, pointsNotFolding) =
        if (foldDirection == 'y') {
          currentPoints.partition(_._2 > foldValue)
        } else currentPoints.partition(_._1 > foldValue)

      val newPoints: Set[(Int, Int)] = pointsToFold.map {
        case (x, y) if foldDirection == 'y' => x -> (foldValue - (y - foldValue))
        case (x, y) if foldDirection == 'x' => (foldValue - (x - foldValue)) -> y
      }

      pointsNotFolding.concat(newPoints)
    }

    def part1(): Unit = {
      val finalPoints = (points, foldInstructions.head) match {
        case (currentPoints, (foldDirection, foldValue)) =>
          foldSheet(currentPoints, foldDirection, foldValue)
      }

      println(s"Part1 = ${finalPoints.size}")
    }

    def part2(): Unit = {

      val finalPoints = foldInstructions.foldLeft(points) {
        case (currentPoints, (foldDirection, foldValue)) => foldSheet(currentPoints, foldDirection, foldValue)
      }

      val x = finalPoints.maxBy(_._1)._1
      val y = finalPoints.maxBy(_._2)._2

      val grid = Vector.fill(y + 1)(Vector.fill[String](x + 1)(" "))
      finalPoints
        .foldLeft(grid) {
          case (currentGrid, (x, y)) => currentGrid.updated(y, currentGrid(y).updated(x, "#"))
        }
        .foreach(line => println(line.mkString))
    }

    part1()
    part2()
  }
}
