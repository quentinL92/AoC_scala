package AoC.year2015

import AoC.AoCDay

object Day02 extends AoCDay(2015, 2) {

  val Dimensions = """(\d+)x(\d+)x(\d+)""".r
  val (paper, ribbon) = getLines().collect {
    case Dimensions(l, w, h) => (l.toInt, w.toInt, h.toInt)
  }.foldLeft(0 -> 0) {
    case ((paper, ribbon), (l, w, h)) =>
      (
        paper + (2 * l * w + 2 * l * h + 2 * w * h + List(l, w, h).sorted.take(2).product),
        ribbon + List(l, w, h).sorted.take(2).sum * 2 + l * w * h
      )
  }
  println(s"Paper: $paper")
  println(s"Ribbon: $ribbon")
}
