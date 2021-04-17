package coding_practices.model.graph

case class Graph[T](
  adjacencyLists: Array[AdjacencyList[T]],
)
