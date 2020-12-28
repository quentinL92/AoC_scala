package AoC.datastructure.graph

case class Graph[V, W](
                        vertices: Vector[V],
                        edges: Map[Int, List[Edge[W]]] = Map.empty[Int, List[Edge[W]]]
                      )(implicit evOrd: W => Ordered[W]) {


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

  def addEdgeBidirectional(edge: Edge[W]): Graph[V, W] = {
    println("Add edge Bidirectional")
    addEdge(edge).addEdge(edge.reversed())
  }

  def addEdgeBidirectionalByIndices(from: Int, to: Int, weight: W): Graph[V, W] = {
    println("Add edge Bidirectional by indices")
    addEdgeByIndices(from, to, weight).addEdgeByIndices(to, from, weight)
  }

  def addEdgeBidirectionalByVertices(from: V, to: V, weight: W): Graph[V, W] = {
    println("Add edge Bidirectional by vertices")
    addEdgeByVertices(from, to, weight).addEdgeByVertices(to, from, weight)
  }

  def vertexAt(index: Int): V =
    vertices(index)

  def indexOfVertex(vertex: V): Int =
    vertices.indexOf(vertex)

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
}

object Graph {
  def apply[V, W](
                   vertices: Set[V],
                   edges: Map[Int, List[Edge[W]]] = Map.empty[Int, List[Edge[W]]]
                 )(implicit evOrd: W => Ordered[W]): Graph[V, W] =
    new Graph(vertices.toVector, edges)(evOrd)
}
