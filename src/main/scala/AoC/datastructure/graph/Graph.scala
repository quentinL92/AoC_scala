package AoC.datastructure.graph

import scala.collection.immutable.Queue

case class Graph[V, W](
                        vertices: Vector[V],
                        edges: Map[Int, List[Edge[W]]] = Map.empty[Int, List[Edge[W]]]
                      )(implicit evOrd: W => Ordered[W], n: Numeric[W]) {

  def addVertex(vertex: V): Graph[V, W] = copy(vertices = vertices :+ vertex)

  def addEdge(edge: Edge[W]): Graph[V, W] =
    copy(edges = edges.updated(edge.from, edge :: edges.getOrElse(edge.from, Nil)))

  def addEdgeByIndices(from: Int, to: Int, weight: W): Graph[V, W] = {
    val edge: Edge[W] = Edge(from, to, weight)
    addEdge(edge)
  }

  def addEdgeByVertices(from: V, to: V, weight: W): Graph[V, W] = {
    val fromIndex = vertices.indexOf(from)
    val toIndex = vertices.indexOf(to)
    addEdgeByIndices(fromIndex, toIndex, weight)
  }

  def addEdgeBidirectional(edge: Edge[W]): Graph[V, W] =
    addEdge(edge).addEdge(edge.reversed())

  def addEdgeBidirectionalByIndices(from: Int, to: Int, weight: W): Graph[V, W] =
    addEdgeByIndices(from, to, weight).addEdgeByIndices(to, from, weight)

  def addEdgeBidirectionalByVertices(from: V, to: V, weight: W): Graph[V, W] =
    addEdgeByVertices(from, to, weight).addEdgeByVertices(to, from, weight)

  def vertexAt(index: Int): V =
    vertices(index)

  def indexOfVertex(vertex: V): Int =
    vertices.indexOf(vertex)

  def edgeFromToWith(from: Int, to: Int, weight: W): Option[Edge[W]] =
    edgesForIndex(from).find(edge => edge.to == to && edge.weight == weight)

  def edgeFromToWith(from: V, to: V, weight: W): Option[Edge[W]] =
    edgeFromToWith(indexOfVertex(from), indexOfVertex(to), weight)

  def edgesForIndex(index: Int): List[Edge[W]] =
    edges.getOrElse(index, Nil)

  def edgesForVertex(vertex: V): List[Edge[W]] =
    edges.getOrElse(indexOfVertex(vertex), Nil)

  def neighborsForIndexWithWeights(index: Int): List[(V, W)] =
    edgesForIndex(index).map(edge => (vertexAt(edge.to), edge.weight))

  def neighborsForVertexWithWeights(vertex: V): List[(V, W)] =
    edgesForVertex(vertex).map(edge => (vertexAt(edge.to), edge.weight))

  override def toString: String =
    vertices.map(v => s"v -> ${neighborsForVertexWithWeights(v)}").mkString("\n")

  def shortesMultiSourcePathUsingAllVerticesWithWeight: (Path[W], W) =
    allMultiSourcePathsUsingAllVerticesWithWeight.minBy(_._2)

  def longestMultiSourcePathUsingAllVerticesWithWeight: (Path[W], W) =
    allMultiSourcePathsUsingAllVerticesWithWeight.maxBy(_._2)

  private def allMultiSourcePathsUsingAllVerticesWithWeight: Set[(Path[W], W)] = {
    var paths: Set[(Path[W], W)] = Set.empty[(Path[W], W)]
    val wantedPathLength: Int = vertices.length
    println(s"Wanted visited length = $wantedPathLength")

    def findPaths(visitedVertices: Set[V], lastVisitedVertex: V, currentPath: Queue[Edge[W]] = Queue.empty[Edge[W]], currentWeight: W = n.zero): Unit = {
      val neighborsVertices = neighborsForVertexWithWeights(lastVisitedVertex).filterNot(visitedVertices contains _._1)
      neighborsVertices match {
        case Nil =>
          if (visitedVertices.size == wantedPathLength) paths = paths + (currentPath.toList -> currentWeight)
          else ()
        case list =>
          list collect {
            case (nextVertex: V, nextWeight: W) =>
              edgeFromToWith(lastVisitedVertex, nextVertex, nextWeight) match {
                case Some(value) =>
                  findPaths(visitedVertices + nextVertex, nextVertex, currentPath.appended(value), n.plus(currentWeight, nextWeight))
                case None => ()
              }
          }
        case _ => ()
      }
    }

    for (startingVertex <- vertices) {
      findPaths(Set(startingVertex), startingVertex)
    }
    paths
  }
}
