package AoC.year2015

import AoC.AoCDay

object Day13 extends AoCDay(2015, 13) {

  override lazy val testMode: Boolean = false

  lazy val GainRegex = """([a-zA-Z]+) would gain (\d+) happiness units by sitting next to ([a-zA-Z]+)\.\s?""".r
  lazy val LossRegex = """([a-zA-Z]+) would lose (\d+) happiness units by sitting next to ([a-zA-Z]+)\.\s?""".r

  lazy val lines = getLines
  lazy val mapLove: Map[String, Map[String, Int]] = lines
    .collect {
      case GainRegex(a, amount, b) => (a, amount.toInt, b)
      case LossRegex(a, amount, b) => (a, -amount.toInt, b)
      case other =>
        writeLine(s"Unmatched string: $other")
        ("", 0, "")
    }
    .groupBy(_._1)
    .map {
      case (name, neighbours) =>
        name -> neighbours.map(n => n._3 -> n._2).sortBy(_._2).toMap
    }

  mapLove
    .foreach {
      case (name, neighbours) =>
        writeLine(s"${name.toUpperCase()}")
        neighbours.foreach(neighbour => writeLine(s"""  ${neighbour._1} => ${neighbour._2}"""))
        writeLine("")
    }

  writeLine("")
  writeLine(s"MapLove size = ${mapLove.size}")

  def computeHappiness(placedPerson: Vector[String], happinessAcc: Int, happinessScores: List[Int]): List[Int] = {
    if (placedPerson.size == mapLove.size) {
      mapLove(placedPerson.head).find {
        case (name, _) => name == placedPerson.last
      } match {
        case Some((neighbourName, happinessCost)) =>
          val happinessScore = happinessAcc + happinessCost + mapLove(neighbourName)(placedPerson.head)
          writeLine(s"$placedPerson => $happinessScore")
          happinessScore :: happinessScores
        case _ =>
          println(
            s"Unexpected error: ${placedPerson.head} does not have a preference with " +
              s"${placedPerson.last} as neighbour")
          happinessScores
      }
    } else {
      happinessScores ::: mapLove(placedPerson.head)
        .filter {
          case (name, _) if placedPerson.contains(name) => false
          case _                                        => true
        }
        .toList
        .flatMap {
          case (neighbourName, happinessCost) =>
            val happinessScore = happinessAcc + happinessCost + mapLove(neighbourName)(placedPerson.head)
            computeHappiness(placedPerson.prepended(neighbourName), happinessScore, happinessScores)
        }

    }
  }

  lazy val res = mapLove
    .flatMap {
      case (name, _) =>
        computeHappiness(Vector(name), 0, Nil)
    }

  override def resolveDay(): Unit = println(s"Best happiness Score: ${res.max}")

}
