package AoC.year2015

import AoC.AoCDay

import scala.annotation.tailrec

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

  val res1: String = (0 until 40).foldLeft(input) {
    case (acc, _) => iterate(acc.toList)
  }
  println(s"Res1: ${res1.length}")

}
