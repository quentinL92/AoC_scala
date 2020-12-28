package AoC.year2015

import AoC.AoCDay

import scala.annotation.tailrec

object Day03 extends AoCDay(2015, 3) {
  @tailrec
  def moove(
             nextMooves: List[Char],
             visited: Map[Int, Set[Int]] = Map(0 -> Set(0)),
             position: (Int, Int) = (0, 0)
           ): Int = {
    val (x, y) = position
    //    println(s"Visited places: $visited")
    nextMooves match {
      case Nil => visited.values.foldLeft(0)(_ + _.size)
      case '<' :: tail =>
        val (xNext, yNext) = (x - 1, y)
        moove(tail, visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext), (xNext, yNext))
      case '>' :: tail =>
        val (xNext, yNext) = (x + 1, y)
        moove(tail, visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext), (xNext, yNext))
      case '^' :: tail =>
        val (xNext, yNext) = (x, y - 1)
        moove(tail, visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext), (xNext, yNext))
      case 'v' :: tail =>
        val (xNext, yNext) = (x, y + 1)
        moove(tail, visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext), (xNext, yNext))
    }
  }

  val res = moove(getLine().toList)
  println(res)

  @tailrec
  def moove2(
              nextMooves: List[Char],
              visited: Map[Int, Set[Int]] = Map(0 -> Set(0)),
              santaPosition: (Int, Int) = (0, 0),
              robotPosition: (Int, Int) = (0, 0),
              santasTurn: Boolean = true
            ): Int = {
    val (x, y) = if (santasTurn) santaPosition else robotPosition
    //    println(s"Visited places: $visited")
    nextMooves match {
      case Nil => visited.values.foldLeft(0)(_ + _.size)
      case list =>
        val (nextPosition, nextVisited) = list.head match {
          case '<' =>
            val (xNext, yNext) = (x - 1, y)
            (
              (xNext, yNext),
              visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext)
            )
          case '>' =>
            val (xNext, yNext) = (x + 1, y)
            (
              (xNext, yNext),
              visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext)
            )
          case '^' =>
            val (xNext, yNext) = (x, y - 1)
            (
              (xNext, yNext),
              visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext)
            )
          case 'v' =>
            val (xNext, yNext) = (x, y + 1)
            (
              (xNext, yNext),
              visited.updated(xNext, visited.getOrElse(xNext, Set.empty[Int]) + yNext)
            )
        }
        if (santasTurn) moove2(list.tail, nextVisited, nextPosition, robotPosition, !santasTurn)
        else moove2(list.tail, nextVisited, santaPosition, nextPosition, !santasTurn)
    }
  }

  val res2 = moove2(getLine().toList)
  println(res2)
}
