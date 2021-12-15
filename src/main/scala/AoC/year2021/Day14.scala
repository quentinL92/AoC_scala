package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day14 extends AoCDay(2021, 14) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    lazy val lines = getLines

    val template = lines.head.trim

    val PairRegex = """(\w+)\s*->\s*(\w+)""".r

    val pairInsertion: Map[String, Char] = lines.tail
      .map(_.trim)
      .filter(_.nonEmpty)
      .map {
        case PairRegex(key, value) => key -> value.head
      }
      .toMap

    def part1(): Unit = {}

    def part2(): Unit = {
      lazy val maxIterations: Int = 40

      def getpairs(str: String): Map[String, Long] = str.toList match {
        case Nil | _ :: Nil => Map.empty[String, Long]
        case list           => list.sliding(2).map(_.mkString).toList.groupBy(s => s).view.mapValues(_.length.toLong).toMap
      }

      @tailrec
      def compute(
          currentPairs: Map[String, Long],
          charsInString: Map[Char, Long],
          currentIteration: Int = 0
      ): Map[Char, Long] = {
        if (currentIteration <= 4) {
          display(s"Step $currentIteration")
          display(s"charsInString = $charsInString")
        }

        @tailrec
        def computeOneStep(
            remainingString: List[(String, Long)],
            charsInString: Map[Char, Long],
            currentPairs: Map[String, Long] = Map.empty[String, Long]
        ): (Map[String, Long], Map[Char, Long]) = {
          remainingString match {
            case Nil => currentPairs -> charsInString

            case (pair, count) :: tail =>
              val (currentPairsAfterStep, currentCharsAfterStep) = pairInsertion.get(pair) match {
                case Some(charToInsert) =>
                  val pairLeft = s"${pair.head}$charToInsert"
                  val pairRight = s"$charToInsert${pair.last}"
                  currentPairs
                    .updated(pairLeft, currentPairs.getOrElse[Long](pairLeft, 0) + count)
                    .updated(pairRight, currentPairs.getOrElse[Long](pairRight, 0) + count) ->
                    charsInString.updated(charToInsert, charsInString.getOrElse[Long](charToInsert, 0) + count)

                case None =>
                  currentPairs.updated(pair, currentPairs.getOrElse[Long](pair, 0) + count) ->
                    charsInString
              }
              computeOneStep(tail, currentCharsAfterStep, currentPairsAfterStep)
          }
        }

        if (currentIteration == maxIterations) {
          charsInString
        } else {
          val (pairsAfterStep, charsAfterStep) = computeOneStep(currentPairs.toList, charsInString)
          compute(
            pairsAfterStep,
            charsAfterStep,
            currentIteration + 1
          )
        }
      }

      val initialPairs: Map[String, Long] = getpairs(template)
      val initialChars: Map[Char, Long] = template.groupBy(c => c).view.mapValues(_.length.toLong).toMap
      val finalPairs: Map[Char, Long] = compute(initialPairs, initialChars)
      display(finalPairs.mkString(" , "))
      val max = finalPairs.maxBy(_._2)._2
      val min = finalPairs.minBy(_._2)._2

      println(s"Part2 = ${max - min}")

    }

//    part1()
    part2()
  }
}
