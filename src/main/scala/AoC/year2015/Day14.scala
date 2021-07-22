package AoC.year2015

import AoC.AoCDay

object Day14 extends AoCDay(2015, 14) {

  lazy val testMode: Boolean = false
  lazy val lines = getLines(test = testMode)
  lazy val duration = if (testMode) 1000 else 2503

  lazy val ReindeerRegex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Reindeer(name: String, velocity: Int, duration: Int, breakDuration: Int, score: Int = 0) {
    lazy val wholeDuration: Int = duration + breakDuration
  }

  lazy val reindeers = lines.collect {
    case ReindeerRegex(name, velocity, duration, breakDuration) =>
      Reindeer(name, velocity.toInt, duration.toInt, breakDuration.toInt)
  }

  lazy val reindeersDistance = reindeers.map(computeDistance(_, duration))

  private def computeDistance(reindeer: Reindeer, iteration: Int): Int = {
    val fullSteps = iteration / reindeer.wholeDuration
    val remainingSeconds = iteration % reindeer.wholeDuration
    (
      (fullSteps * reindeer.duration) +
        (if (remainingSeconds > reindeer.duration) reindeer.duration else remainingSeconds)
    ) * reindeer.velocity
  }

  lazy val reindeersScore = (1 to duration).foldLeft(reindeers.map(reindeer => reindeer.name -> 0)) {
    case (acc, currentSecond) =>
      val reindeersDistance = reindeers.map(reindeer => reindeer.name -> computeDistance(reindeer, currentSecond))
      val reindeerBestDistance = reindeersDistance.maxBy(_._2)._2
      val reindeersWinning = reindeersDistance.filter(_._2 == reindeerBestDistance).map(_._1)
      acc.map {
        case (name, score) if reindeersWinning.contains(name) => name -> (score + 1)
        case other                                            => other
      }
  }

  override def resolveDay(): Unit = {
//    println(s"The max distance is: ${reindeersDistance.max}")
    println(s"The winning reindeer have: ${reindeersScore.maxBy(_._2)._2} points")
  }
}
