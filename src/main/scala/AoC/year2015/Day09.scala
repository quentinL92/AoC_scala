package AoC.year2015

import AoC.AoCDay
import AoC.datastructure.graph.Graph

import scala.util.matching.Regex

object Day09 extends AoCDay(2015, 9) {

  override lazy val testMode: Boolean = false

  lazy val lines: Vector[String] = getLines
  lazy val TownRegex: Regex = """(\w+) to (\w+) = \d+""".r
  lazy val EdgeRegex: Regex = """(\w+) to (\w+) = (\d+)""".r
  lazy val towns: Set[String] = lines.flatMap { case TownRegex(town1, town2) => List(town1, town2) }.toSet

  lazy val graph: Graph[String, Int] = lines.foldLeft[Graph[String, Int]](Graph[String, Int](towns.toVector)) {
    case (g, EdgeRegex(town1, town2, weight)) => g.addEdgeBidirectionalByVertices(town1, town2, weight.toInt)
  }

  override def resolveDay(): Unit = {
    val shortestPath = graph.shortesMultiSourcePathUsingAllVerticesWithWeight
    println(s"ShortestPath: ${shortestPath._1} with weight: ${shortestPath._2}")

    val longestPath = graph.longestMultiSourcePathUsingAllVerticesWithWeight
    println(s"LongestPath: ${longestPath._1} with weight: ${longestPath._2}")
  }

}
