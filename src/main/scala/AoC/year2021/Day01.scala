package AoC.year2021

import AoC.AoCDay

object Day01 extends AoCDay(2021, 1) {

  override lazy val testMode: Boolean = false

  lazy val lines: Vector[Int] = getLines.map(_.toInt)

  def part1(): Unit =
    println(lines.sliding(2).count {
      case Seq(a, b) => b > a
    })

  def part2(): Unit =
    println(
      lines
        .sliding(3)
        .map(_.sum)
        .sliding(2)
        .count {
          case Seq(a, b) => b > a
        }
    )

  override def resolveDay(): Unit = part2()
}
