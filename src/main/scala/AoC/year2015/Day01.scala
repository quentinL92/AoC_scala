package AoC.year2015

import AoC.AoCDay

object Day01 extends AoCDay(2015, 1) {
  lazy val line: String = getLine()
  lazy val res: Int = line.toList.foldLeft(0) {
    case (acc, cur) if cur == '(' => acc + 1
    case (acc, cur) if cur == ')' => acc - 1
  }

  lazy val res2 = line
    .scanLeft(0) {
      case (acc, cur) if cur == '(' => acc + 1
      case (acc, cur) if cur == ')' => acc - 1
    }
    .indexWhere(_ < 0)

  override def resolveDay(): Unit = {
    println(res)
    println("-------------")
    println(res2)
  }
}
