package AoC.datastructure.graph

case class Edge[W] (from: Int, to: Int, weight: W)(implicit ev: W => Ordered[W]) extends Ordered[Edge[W]] {
  def reversed(): Edge[W] = copy(from = to, to = from, weight)

  override def compare(that: Edge[W]): Int = weight.compare(that.weight)

  override def toString: String = s"$from -$weight-> $to"
}