package AoC.year2023

import AoC.AoCDay

object Day02 extends AoCDay(2023, 2) {
  override def testMode: Boolean = false

  sealed trait Color {
    def number: Long
    def color: String
  }

  final case class Red(number: Long) extends Color {
    override val color: String = "red"
    def +(other: Red): Red = copy(number + other.number)
  }

  final case class Green(number: Long) extends Color {
    override val color: String = "green"
    def +(other: Green): Green = copy(number + other.number)
  }

  final case class Blue(number: Long) extends Color {
    override val color: String = "blue"
    def +(other: Blue): Blue = copy(number + other.number)
  }

  final case class CubeSet(
      red: Long,
      green: Long,
      blue: Long
  ) {
    def +(other: CubeSet): CubeSet = {
      copy(
        red = Math.max(red, other.red),
        green = Math.max(green, other.green),
        blue = Math.max(blue, other.blue)
      )
    }

    def <=(other: CubeSet): Boolean =
      red <= other.red &&
        green <= other.green &&
        blue <= other.green

  }

  object CubeSet {
    def apply(red: Red, green: Green, blue: Blue): CubeSet = CubeSet(
      red.number,
      green.number,
      blue.number
    )

    def zero: CubeSet = CubeSet(0, 0, 0)
  }

  def part1(): Unit = {

    val targetCubeSet = CubeSet(
      red = 12,
      green = 13,
      blue = 14
    )

    val GameIdRegex = """^Game (\d+)$""".r
    val BlueRegex = """(\d+) blue""".r
    val RedRegex = """(\d+) red""".r
    val GreenRegex = """(\d+) green""".r

    val res = getLines
      .map { line =>
        val splittedGame = line.split(':')

        val GameIdRegex(gameId) = splittedGame.head

        val maxCubeSet = splittedGame.last
          .strip()
          .split(';')
          .map(_.strip())
          .map { set =>
            val colorsInSet: Array[Color] = set
              .split(',')
              .map(_.strip())
              .collect[Color] {
                case RedRegex(redNumber)     => Red(redNumber.toLong)
                case GreenRegex(greenNumber) => Green(greenNumber.toLong)
                case BlueRegex(blueNumber)   => Blue(blueNumber.toLong)
              }

            CubeSet(
              colorsInSet.collect { case Red(number)   => number }.fold(0L)(_ + _),
              colorsInSet.collect { case Green(number) => number }.fold(0L)(_ + _),
              colorsInSet.collect { case Blue(number)  => number }.fold(0L)(_ + _),
            )
          }
          .fold[CubeSet](CubeSet.zero)(_ + _)

        gameId.toLong -> maxCubeSet

      }
//      .filter { case (_, resultingCubeSet) => resultingCubeSet <= targetCubeSet }
//      .map(_._1)
//      .sum

    println(res.map(truc => s"${truc._1} -> ${truc._2}\t=> ${truc._2 <= targetCubeSet}").mkString("\n"))
  }
  def part2(): Unit = ???

  override def resolveDay(): Unit = part1()
}
