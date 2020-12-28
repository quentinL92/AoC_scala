import AoC.datastructure.graph.{Graph, Path}
import AoC.datastructure.search.Dijkstra.{DijkstraResult, shortestPathsFrom}
import AoC.year2015.Day09.getLines

import scala.util.matching.Regex

val lines: Vector[String] = Vector(
  "London to Dublin = 464",
    "London to Belfast = 518",
    "Dublin to Belfast = 141"
)
val TownRegex: Regex = """(\w+) to (\w+) = \d+""".r
val towns: Set[String] = lines
  .flatMap { case TownRegex(town1, town2) => List(town1, town2)}
  .toSet