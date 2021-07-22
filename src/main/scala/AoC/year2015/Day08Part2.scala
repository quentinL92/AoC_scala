package AoC.year2015

import AoC.AoCDay

object Day08Part2 extends AoCDay(2015, 8) {
  lazy val lines = getLines()

  def countChars(currentCount: (Int, Int), string: String): (Int, Int) = {
    val (realCount, representationCount) = currentCount
    val newRepresentation: String = "\"" + string
      .replace("\\", "\\\\")
      .replace("\"", "\\\"") + "\""

    println(s"Before $string")
    println(s"After $newRepresentation")
    (realCount + string.length, representationCount + newRepresentation.length)
  }

  lazy val (realNbChars, representationNbChars) = lines.foldLeft((0, 0))(countChars)
  lazy val res2 = representationNbChars - realNbChars

  override def resolveDay(): Unit = println(s"Res2: $representationNbChars - $realNbChars = $res2")

//  2355 too high

}
