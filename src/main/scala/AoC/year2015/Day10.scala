package AoC.year2015

import AoC.AoCDay

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day10 extends AoCDay(2015, 10) {
  val input: String = "3113322113"
  @tailrec
  def iterate(chars: List[Char], result: String = ""): String = chars match {
    case Nil => result
    case head :: Nil => result + s"1$head"
    case head :: _ =>
      val nextIndex: Int = chars.indexWhere(_ != head)
      iterate(chars.drop(nextIndex), result + s"$nextIndex$head")
  }

  val line: String = input
  val DigitSequenceRegex: Regex = """(\d)\1*""".r
  val res2 = (0 until 50).foldLeft(line) {
    case (acc, _) => DigitSequenceRegex.replaceAllIn(acc, m => s"${m.matched.length}${m.matched.charAt(0)}")
  }
  println(res2.length)
}
