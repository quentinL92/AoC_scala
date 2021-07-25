package AoC.year2015

import AoC.AoCDay

object Day17 extends AoCDay(2015, 17) {

  lazy val testMode: Boolean = false
  lazy val liters: Int = if (testMode) 25 else 150

  // The trick is to zip the collection with its index so combinations will work as expected here
  lazy val fridges: List[(Int, Int)] =
    getLines(test = testMode).toList.zipWithIndex
      .map { case (fridgeCapacity, fridgeIndex) => fridgeCapacity.toInt -> fridgeIndex }

  def combine(
      n: Int,
      fridgesToUse: List[(Int, Int)]
  ): Iterator[List[Int]] = fridgesToUse.combinations(n).map(_.map(_._1))

  lazy val possibilities: IndexedSeq[List[Int]] = (1 to fridges.size)
    .flatMap(combine(_, fridges).filter(_.sum == liters))

  lazy val minimumFridgeRequired = possibilities.minBy(_.size).size
  lazy val possibilitiesUsingMinimumFridges = possibilities.filter(_.size == minimumFridgeRequired)

  override def resolveDay(): Unit = {
    println(s"Part1: ${possibilities.size}")
    println(s"Part2: ${possibilitiesUsingMinimumFridges.size}")
  }
}
