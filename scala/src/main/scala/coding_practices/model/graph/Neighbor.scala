package coding_practices.model.graph

case class Neighbor[T](
  vertex: T,
  weight: Int,
)
