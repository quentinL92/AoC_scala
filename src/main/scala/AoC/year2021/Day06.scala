package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day06 extends AoCDay(2021, 6) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    val line = getLine.split(',').map(_.toInt).toList

    def part1(): Unit = {
      val numberOfDays = 80
      println(s"Initial state $line")

      @tailrec
      def inner(fishes: List[Int], remainingDays: Int = numberOfDays): List[Int] = {
        if (remainingDays == 0) fishes
        else {
          val (fishesReset, other) = fishes.map(_ - 1).partition(_ == -1)
          val newList = List.fill(fishesReset.length)(8) ::: List.fill(fishesReset.length)(6) ::: List.from(other)
          inner(
            newList,
            remainingDays - 1
          )
        }
      }

      println(s"Part1 = ${inner(line).length}")

    }

    def part2(): Unit = {
      val numberOfDays = 256
      @tailrec
      def inner(fishes: Map[Int, Long], remainingDays: Int = numberOfDays): Map[Int, Long] = {
        if (remainingDays == 0) fishes
        else {
          val newMap = fishes.map {
            case (key, numberOfFishes) => (key - 1) -> numberOfFishes
          }
          val numberOfResetingFishes = newMap.getOrElse(-1, 0L)
          inner(
            newMap
              .updated(8, numberOfResetingFishes + newMap.getOrElse(8, 0L))
              .updated(6, numberOfResetingFishes + newMap.getOrElse(6, 0L))
              .view
              .filterKeys(_ != -1)
              .toMap,
            remainingDays - 1
          )
        }
      }

      println(s"Part 2 = ${inner(line.groupBy(i => i).view.mapValues(_.length.toLong).toMap).toList.map(_._2).sum}")
    }

    part1()
    part2()

  }
}
