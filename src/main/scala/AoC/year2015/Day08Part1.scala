package AoC.year2015

import AoC.AoCDay

import scala.annotation.tailrec

object Day08Part1 extends AoCDay(2015, 8) {

  override lazy val testMode: Boolean = false

  lazy val lines = getLines
  lazy val HexaRegex = """(\\x[a-f0-9]{2})""".r

  def countChars(currentCount: (Int, Int), string: String): (Int, Int) = {
    val (curr, inMemoreCurr) = currentCount

    def hexaToString(hexa: String): String =
      Integer
        .parseInt(hexa.drop(2), 16)
        .toChar
        .toString

    @tailrec
    def replaceEscapeChars(string: String): String = {
      if (HexaRegex.findFirstMatchIn(string).isDefined) {
        val matches = HexaRegex.findAllIn(string).toList
        replaceEscapeChars(matches.fold(string)((acc, cur) => acc.replace(cur, hexaToString(cur))))
      } else string
    }

    val stringWithoutEscape: String = replaceEscapeChars(
      string
        .dropRight(1)
        .drop(1)
        .replace("\\\\", "\\")
        .replace("\\\"", "\"")
    )
    (curr + string.length, inMemoreCurr + stringWithoutEscape.length)
  }

  lazy val (nbChars, inMemoreNbChars) = lines.foldLeft((0, 0))(countChars)

  lazy val res1 = nbChars - inMemoreNbChars

  override def resolveDay(): Unit = println(s"Res1: $nbChars - $inMemoreNbChars = $res1")

}
