package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day07 extends AoCDay(2021, 7) {
  override def resolveDay(): Unit = {

    val line = getLine().split(',').map(_.toInt).toList

    val minPos = line.min
    val maxPos = line.max

    def part1(): Unit = {

      val (pos, minFuelConsumption) = (minPos to maxPos).zipWithIndex
        .map {
          case (pos, index) =>
            index -> line.foldLeft(0) {
              case (fuelConsumption, initialPos) => fuelConsumption + Math.abs(initialPos - pos)
            }
        }
        .minBy(_._2)

      println(s"Optimal pos = $pos with fuel consumption = $minFuelConsumption")
    }

    def part2(): Unit = {

      @tailrec
      def computeFuel(distance: Int, currentMovement: Int = 0, fuelConsumption: Int = 0): Int =
        if (distance == 0) fuelConsumption
        else {
          val nextMovement = currentMovement + 1
          computeFuel(distance - 1, nextMovement, fuelConsumption + nextMovement)
        }

      val (pos, minFuelConsumption) = (minPos to maxPos).zipWithIndex
        .map {
          case (pos, index) =>
            index -> line.foldLeft(0) {
              case (fuelConsumption, initialPos) => fuelConsumption + computeFuel(Math.abs(pos - initialPos))
            }
        }
        .minBy(_._2)

      println(s"Optimal pos = $pos with fuel consumption = $minFuelConsumption")
    }

    part1()
    part2()
  }
}
