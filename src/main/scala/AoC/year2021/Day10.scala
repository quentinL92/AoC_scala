package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day10 extends AoCDay(2021, 10) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    lazy val lines = getLines.map(_.trim)

    val errorPoints: Map[Char, Long] = Map(
      ')' -> 3,
      ']' -> 57,
      '}' -> 1197,
      '>' -> 25137
    )

    val completionPoints: Map[Char, Long] = Map(
      ')' -> 1,
      ']' -> 2,
      '}' -> 3,
      '>' -> 4
    )

    def matchingParenthesis(opening: Char, current: Char): Boolean = (opening, current) match {
      case '(' -> ')' => true
      case '[' -> ']' => true
      case '{' -> '}' => true
      case '<' -> '>' => true
      case _          => false
    }

    def getMatchingClosingParenthesis(opening: Char): Char = opening match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }

    def isClosingParenthesis(parenthesis: Char): Boolean =
      List(')', ']', '}', '>').contains(parenthesis)
    def isOpeningParenthesis(parenthesis: Char): Boolean =
      List('(', '[', '{', '<').contains(parenthesis)

    def part1(): Unit = {

      @tailrec
      def process(remainingElements: List[Char], parenthesisAcc: Queue[Char] = Queue.empty[Char]): Option[Long] = {
        remainingElements match {
          case Nil => None
          case head :: tail if isClosingParenthesis(head) && matchingParenthesis(parenthesisAcc.head, head) =>
            process(tail, parenthesisAcc.dequeue._2)

          case head :: _ if isClosingParenthesis(head) =>
            Some(errorPoints.getOrElse(head, 0))

          case head :: tail if isOpeningParenthesis(head) => process(tail, parenthesisAcc.prepended(head))
          case _ :: tail                                  => process(tail, parenthesisAcc)
        }
      }

      val res = lines
        .map(_.toList)
        .map(process(_))
        .collect {
          case Some(value) => value
        }
        .sum

      println(s"Part1 = $res")

    }

    def part2(): Unit = {
      @tailrec
      def process(remainingElements: List[Char], parenthesesAcc: Queue[Char] = Queue.empty[Char], score: Long = 0): Option[Long] = {
        remainingElements match {
          case Nil if parenthesesAcc.isEmpty =>
            if (score != 0) Some(score) else None

          case Nil =>
            val nextClosing = getMatchingClosingParenthesis(parenthesesAcc.head)
            process(Nil, parenthesesAcc.dequeue._2, score * 5L + completionPoints.getOrElse(nextClosing, 0L))

          case head :: tail if isClosingParenthesis(head) && matchingParenthesis(parenthesesAcc.head, head) =>
            process(tail, parenthesesAcc.dequeue._2)

          case head :: _ if isClosingParenthesis(head) =>
            None

          case head :: tail if isOpeningParenthesis(head) => process(tail, parenthesesAcc.prepended(head))
          case _ :: tail                                  => process(tail, parenthesesAcc)
        }
      }

      val res = lines
        .map(_.toList)
        .map(process(_))
        .collect {
          case Some(value) => value
        }
        .sorted

      println(res)
      println(s"Part2 = ${res(res.length / 2)}")
    }

//    part1()
    part2()

  }
}
