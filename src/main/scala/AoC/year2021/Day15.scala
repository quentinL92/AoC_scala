package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day15 extends AoCDay(2021, 15) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    case class Location(x: Int, y: Int)
    val start: Location = Location(0, 0)

    lazy val lines = getLines

    val grid: Map[Location, Int] = lines.zipWithIndex.flatMap {
      case (str, y) =>
        str.toVector.zipWithIndex.map {
          case (c, x) => Location(x, y) -> Integer.parseInt(c.toString)
        }
    }.toMap

    def dijkstra(start: Location, end: Location, grid: Map[Location, Int]): Int = {

      def getNeighbours(location: Location, xMax: Int = end.x, yMax: Int = end.y): List[Location] = {
        val (x, y) = location.x -> location.y
        def guard(xOther: Int, yOther: Int): Boolean =
          xOther >= 0 && xOther <= xMax &&
            yOther >= 0 && yOther <= yMax &&
            (x, y) != (xOther, yOther) &&
            (xOther == x || yOther == y)

        (for {
          xOther <- (x - 1) to (x + 1)
          yOther <- (y - 1) to (y + 1) if guard(xOther, yOther)
        } yield Location(xOther, yOther)).toList
      }

      @tailrec
      def dijkstraTailrec(
          locationsPathRisk: Map[Location, Int] = Map(start -> 0),
          alreadyVisited: Set[Location] = Set(start)
      ): Int =
        locationsPathRisk.get(end) match {
          case Some(value) => value
          case None =>
            val (bestCurrentPathLocation, bestCurrentPathRisk) = locationsPathRisk.minBy(_._2)
            val neighbours = getNeighbours(bestCurrentPathLocation)
            val filteredNeighbours = neighbours
              .filterNot(locationsPathRisk.contains)
              .filterNot(alreadyVisited.contains)

            val nextLocationsPathRisk = filteredNeighbours
              .foldLeft(locationsPathRisk.removed(bestCurrentPathLocation)) {
                case (currentLocationsPathRisk, location: Location) =>
                  currentLocationsPathRisk.updated(location, grid.getOrElse(location, 0) + bestCurrentPathRisk)
              }

            dijkstraTailrec(nextLocationsPathRisk, alreadyVisited.incl(bestCurrentPathLocation))
        }

      dijkstraTailrec()
    }

    def part1(): Unit = {

      val endX = grid.keySet.maxBy(_.x).x
      val endY = grid.keySet.maxBy(_.y).y
      val end: Location = Location(endX, endY)

      val res = dijkstra(start, end, grid)
      println(s"Part1 = $res")

    }

    def part2(): Unit = {
      val tileWidth = grid.keySet.maxBy(_.x).x + 1
      val tileHeigth = grid.keySet.maxBy(_.y).y + 1

      val paddedGrid: Map[Location, Int] = (for {
        xFactor <- 0 to 4
        yFactor <- 0 to 4 if (xFactor, yFactor) != (0, 0)
      } yield
        grid.map {
          case (Location(x, y), risk) =>
            val newRisk = risk + xFactor + yFactor
            Location(x + xFactor * tileWidth, y + yFactor * tileHeigth) -> (if (newRisk > 9) newRisk - 9 else newRisk)
        }).foldLeft(grid) {
        case (acc, cur) => acc.concat(cur)
      }

      val endX = paddedGrid.keySet.maxBy(_.x).x
      val endY = paddedGrid.keySet.maxBy(_.y).y
      val end: Location = Location(endX, endY)

      val res = dijkstra(start, end, paddedGrid)
      println(s"Part2 = $res")

    }

//    part1()
    part2()

  }
}
