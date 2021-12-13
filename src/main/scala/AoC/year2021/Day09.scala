package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day09 extends AoCDay(2021, 9) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    case class Point(x: Int, y: Int, value: Int, isLow: Boolean) {
      def toLocation: (Int, Int) = x -> y
    }

    lazy val lines: Vector[String] = getLines

    val grid: Vector[Vector[Point]] = lines.zipWithIndex.map {
      case (y, iy) =>
        y.zipWithIndex.map {
          case (x, ix) =>
            Point(ix, iy, value = Integer.parseInt(x.toString), isLow = false)
        }.toVector
    }

    def part1(): Unit = {

      def isLowPoint(x: Int, y: Int): Boolean = {
        val currValue = grid(y)(x).value

        val tmp = (
          for {
            yOther <- (y - 1) to (y + 1) if yOther >= 0 && yOther < grid.length
            xOther <- (x - 1) to (x + 1) if xOther >= 0 && xOther < grid(y).length
          } yield (xOther, yOther)
        ).collect {
          case (xOther, yOther) if (xOther == x || yOther == y) && (xOther != x || yOther != y) =>
            grid(yOther)(xOther)
        }

        tmp.forall(_.value > currValue)
      }

      val res = grid.zipWithIndex
        .flatMap {
          case (row, rowIndex) =>
            row.zipWithIndex.collect {
              case (element, elementIndex) if isLowPoint(elementIndex, rowIndex) =>
                println(element.value)
                element
            }
        }
        .map(_.value + 1)
        .sum

      println(s"Part1 = $res")
    }

    def part2(): Unit = {

      def getNeighbours(x: Int, y: Int): List[(Int, Int)] =
        (for {
          yOther <- (y - 1) to (y + 1) if yOther >= 0 && yOther < grid.length
          xOther <- (x - 1) to (x + 1) if xOther >= 0 && xOther < grid(y).length && (xOther, yOther) != (x, y) && (xOther == x || yOther == y)
        } yield (xOther, yOther)).toList

      def isLowPoint(x: Int, y: Int): Boolean = {
        val currValue = grid(y)(x).value

        val tmp = getNeighbours(x, y).collect {
          case (xOther, yOther) if (xOther == x || yOther == y) && (xOther != x || yOther != y) =>
            grid(yOther)(xOther)
        }

        tmp.forall(_.value > currValue)
      }

      val lowPoints = grid.zipWithIndex
        .flatMap {
          case (row, rowIndex) =>
            row.zipWithIndex.collect {
              case (element, elementIndex) if isLowPoint(elementIndex, rowIndex) =>
                element
            }
        }

      @tailrec
      def getBassin(
          remainingLocation: List[((Int, Int), Int)],
          membersOfBassin: Set[Point]
      ): Set[Point] = {
        remainingLocation match {
          case Nil => membersOfBassin

          case ((x, y), previousHeight) :: tail =>
            val currentPoint = grid(y)(x)
            val currentHeight = currentPoint.value
            if (currentHeight < 9 && currentHeight > previousHeight) {
              val unvisitedNeighbours = getNeighbours(x, y).filterNot(membersOfBassin.map(_.toLocation).contains)
              getBassin(
                unvisitedNeighbours.map(v => v -> currentHeight) ::: tail,
                membersOfBassin.incl(currentPoint)
              )
            } else {
              getBassin(tail, membersOfBassin)
            }
        }
      }

      val res = lowPoints
        .map {
          case p @ Point(x, y, value, _) =>
            val neighbours = getNeighbours(x, y)
            val neighboursWithValue = neighbours.map(v => v -> value)
            getBassin(neighboursWithValue, Set(p))
        }
        .map(_.size)
        .sorted(Ordering.Int.reverse)
        .take(3)
        .product

      println(s"Part2 = $res")

    }

//    part1()
    part2()

  }
}
