import AoC.datastructure.graph.{Graph, Path}
import AoC.datastructure.search.Dijkstra.{DijkstraResult, shortestPathsFrom}

val graph: Graph[String, Int] = Graph[String, Int](
  Vector("Seattle",
    "San Francisco",
    "Los Angeles",
    "Riverside",
    "Phoenix",
    "Chicago",
    "Boston",
    "New York",
    "Atlanta",
    "Miami",
    "Dallas",
    "Houston",
    "Detroit",
    "Philadelphia",
    "Washington")
)

graph.addEdgeByVertices("Seattle", "Chicago", 1737)
graph.addEdgeByVertices("Seattle", "San Francisco", 678)
graph.addEdgeByVertices("San Francisco", "Riverside", 386)
graph.addEdgeByVertices("San Francisco", "Los Angeles", 348)
graph.addEdgeByVertices("Los Angeles", "Riverside", 50)
graph.addEdgeByVertices("Los Angeles", "Phoenix", 357)
graph.addEdgeByVertices("Riverside", "Phoenix", 307)
graph.addEdgeByVertices("Riverside", "Chicago", 1704)
graph.addEdgeByVertices("Phoenix", "Dallas", 887)
graph.addEdgeByVertices("Phoenix", "Houston", 1015)
graph.addEdgeByVertices("Dallas", "Chicago", 805)
graph.addEdgeByVertices("Dallas", "Atlanta", 721)
graph.addEdgeByVertices("Dallas", "Houston", 225)
graph.addEdgeByVertices("Houston", "Atlanta", 702)
graph.addEdgeByVertices("Houston", "Miami", 968)
graph.addEdgeByVertices("Atlanta", "Chicago", 588)
graph.addEdgeByVertices("Atlanta", "Washington", 543)
graph.addEdgeByVertices("Atlanta", "Miami", 604)
graph.addEdgeByVertices("Miami", "Washington", 923)
graph.addEdgeByVertices("Chicago", "Detroit", 238)
graph.addEdgeByVertices("Detroit", "Boston", 613)
graph.addEdgeByVertices("Detroit", "Washington", 396)
graph.addEdgeByVertices("Detroit", "New York", 482)
graph.addEdgeByVertices("Boston", "New York", 190)
graph.addEdgeByVertices("New York", "Philadelphia", 81)
graph.addEdgeByVertices("Philadelphia", "Washington", 123)

val result: DijkstraResult[String, Int] = shortestPathsFrom(graph, "Los Angeles")
println(s"Distances from Los Angeles: ")
result
  .weightByVertex
  .toList
  .foreach { case (k, v) => println(s"$k: $v") }

println("Shortes path from Los Angeles to Boston:")
val path: Path[Int] = result.pathTo("Los Angeles", "Boston")
println(path)