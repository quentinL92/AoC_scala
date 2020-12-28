package AoC.datastructure.search

import AoC.datastructure.graph.{Edge, Graph, Path}
import AoC.year2015.Day09.path

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Dijkstra {

  case class Node[W](index: Int, weight: W)(implicit ev: W => Ordered[W]) extends Ordered[Node[W]] {
    override def compare(that: Node[W]): Int = weight.compare(that.weight)
  }

  case class DijkstraResult[V, W](start: V, graph: Graph[V, W], distances: Map[Int, W], pathMap: Map[Int, Edge[W]]) {
    lazy val weightByVertex: Map[V, Option[W]] =
      graph.vertices.zipWithIndex.map { case (vertex, idx) => vertex -> distances.get(idx) }.toMap

    def pathTo(to: Int): Path[W] =
      if (pathMap.isEmpty) Nil
      else {
        val startIndex: Int = graph.indexOfVertex(start)
        val path: mutable.ListBuffer[Edge[W]] = ListBuffer()
        var current: Edge[W] = pathMap(to)
        path.prepend(current)
        while (current.from != startIndex) {
          current = pathMap(current.from)
          path.prepend(current)
        }
        path.toList
      }

    def pathTo(to: V): Path[W] = pathTo(graph.indexOfVertex(to))

    def pathToString(path: Path[W]): String =
      path.map(edge => s"${graph.vertexAt(edge.from)} -${edge.weight}-> ${graph.vertexAt(edge.to)}").mkString("\n")
  }

  def shortestPathsFrom[V, W](
                               graph: Graph[V, W],
                               from: V
                             )
                             (
                               implicit ev: W => Ordered[W],
                               n: Numeric[W]
                             ): DijkstraResult[V,W] = {
    val indexOfFirst: Int = graph.indexOfVertex(from)
    val distances: mutable.Map[Int, W] = mutable.Map[Int, W](indexOfFirst -> n.zero)
    val pathMap: mutable.Map[Int, Edge[W]] = mutable.Map.empty
    val pq: mutable.PriorityQueue[Node[W]] =
      mutable.PriorityQueue(Node[W](indexOfFirst, n.zero))((nodeA, nodeB) => nodeA.compare(nodeB) * -1)

    while (pq.nonEmpty) {
      val currentVertexIndex: Int = pq.dequeue().index
      val currentDistance: W = distances(currentVertexIndex)

      def updateDistance(edge: Edge[W], newDistance: W): Unit = {
        distances.put(edge.to, newDistance)
        pathMap.put(edge.to, edge)
        pq.addOne(Node(edge.to, currentDistance))
      }

      for (edge <- graph.edgesForIndex(currentVertexIndex)) {
        val maybeKnownDistance: Option[W] = distances.get(edge.to)
        val newDistance: W = n.plus(edge.weight, currentDistance)
        maybeKnownDistance match {
          case None =>
            updateDistance(edge, newDistance)

          case Some(knownDistance) if knownDistance > newDistance =>
            updateDistance(edge, newDistance)

          case Some(_) => ()
        }
      }
    }
    DijkstraResult(from, graph, distances.toMap, pathMap.toMap)
  }
  val
}
