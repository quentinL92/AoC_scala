package AoC.year2015

import AoC.AoCDay

object Day14 extends AoCDay(2015, 14) {

  lazy val testMode: Boolean = false
  lazy val lines = getLines(test = testMode)
  lazy val duration = if (testMode) 1000 else 2503

  lazy val ReindeerRegex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  case class Reindeer(name: String, velocity: Int, duration: Int, breakDuration: Int) {
    lazy val wholeDuration: Int = duration + breakDuration
  }

  lazy val reindeers = lines.collect {
    case ReindeerRegex(name, velocity, duration, breakDuration) =>
      Reindeer(name, velocity.toInt, duration.toInt, breakDuration.toInt)
  }

  lazy val reindeersDistance = reindeers.map { reindeer =>
    val fullSteps = duration / reindeer.wholeDuration
    val remainingSeconds = duration % reindeer.wholeDuration
    (
      (fullSteps * reindeer.duration) +
        (if (remainingSeconds > reindeer.duration) reindeer.duration else remainingSeconds)
    ) * reindeer.velocity
  }

  override def resolveDay(): Unit = println(s"The max distance is: ${reindeersDistance.max}")
}
