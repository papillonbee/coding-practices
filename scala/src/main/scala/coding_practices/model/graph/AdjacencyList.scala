package coding_practices.model.graph

case class AdjacencyList[T](
  vertex: T,
  neighbors: Seq[Neighbor[T]],
)
