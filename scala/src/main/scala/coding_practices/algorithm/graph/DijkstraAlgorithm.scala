package coding_practices.algorithm.graph

import coding_practices.data_structure.binary_heap.{BinaryHeap, BinaryHeapImpl}
import coding_practices.data_structure.hash_map.{HashMap, HashMapImpl}
import coding_practices.model.graph.{AdjacencyList, Graph, Neighbor}

import scala.annotation.tailrec

case class Node[T](
  adjacencyList: AdjacencyList[T],
  var shortestDistance: Int,
)

case class Record[T](
  adjacencyList: AdjacencyList[T],
  var shortestDistance: Int,
  var pathVia: Option[T],
) {
  def toNode: Node[T] = Node(adjacencyList, shortestDistance)
  def setShortestDistance(distance: Int): Unit = {
    shortestDistance = distance
  }
  def setPathVia(vertex: Option[T]): Unit = {
    pathVia = vertex
  }
}

trait DijkstraAlgorithm[T] {
  def getShortestDistanceFromSourceToVertex(vertex: T): Option[Int]
  def getShortestPathFromSourceToVertex(vertex: T): Seq[T]
}

class DijkstraAlgorithmImpl[T](graph: Graph[T], source: T) extends DijkstraAlgorithm[T] {

  val lookupTable: HashMap[T, Record[T]] = new HashMapImpl[T, Record[T]]()

  val records: Array[Record[T]] = graph.adjacencyLists.map { adjacencyList: AdjacencyList[T] =>
    val vertex: T = adjacencyList.vertex
    val shortestDistance: Int = if (vertex == source) 0 else Int.MaxValue

    val record: Record[T] = Record(
      adjacencyList = adjacencyList,
      shortestDistance = shortestDistance,
      pathVia = None,
    )

    lookupTable.put(vertex, record)

    record
  }

  val orderingByShortestDistance: Ordering[Node[T]] = Ordering.by[Node[T], Int](_.shortestDistance)
  val priorityQueue: BinaryHeap[Node[T]] = new BinaryHeapImpl[Node[T]](
    records.filter(_.adjacencyList.vertex == source).map(_.toNode): _*
  )(orderingByShortestDistance.reverse)

  buildShortestPathFromSource()

  private def buildShortestPathFromSource(): Unit = recursivelyBuildShortestPathFromSource()

  @tailrec
  private def recursivelyBuildShortestPathFromSource(): Unit = {
    priorityQueue.pop() match {
      case Some(node: Node[T]) =>
        val vertex: T = node.adjacencyList.vertex

        val recordOpt: Option[Record[T]] = lookupTable.get(vertex)
        recordOpt match {
          case Some(record: Record[T]) if node.shortestDistance == record.shortestDistance =>

            record.adjacencyList.neighbors.foreach { neighbor: Neighbor[T] =>
              val neighborVertex: T = neighbor.vertex
              val maybeShortestDistance: Int = record.shortestDistance + neighbor.weight
              val neighborRecordOpt: Option[Record[T]] = lookupTable.get(neighborVertex)

              neighborRecordOpt match {
                case Some(neighborRecord: Record[T]) if maybeShortestDistance < neighborRecord.shortestDistance =>

                  neighborRecord.setShortestDistance(maybeShortestDistance)
                  neighborRecord.setPathVia(Some(vertex))

                  lookupTable.put(neighborVertex, neighborRecord)
                  priorityQueue.push(neighborRecord.toNode)
                case _ =>
              }
            }
          case _ =>
        }

        recursivelyBuildShortestPathFromSource()
      case _ =>
    }
  }

  override def getShortestDistanceFromSourceToVertex(vertex: T): Option[Int] = {
    lookupTable.get(vertex).map(_.shortestDistance)
  }

  override def getShortestPathFromSourceToVertex(vertex: T): Seq[T] = {
    recursivelyGetShortestPathFromSourceToVertex(vertex, vertex +: Seq.empty)
  }

  @tailrec
  private def recursivelyGetShortestPathFromSourceToVertex(
    vertex: T,
    path: Seq[T],
  ): Seq[T] = {
    val pathViaOpt: Option[T] = lookupTable.get(vertex).flatMap(_.pathVia)
    pathViaOpt match {
      case Some(pathVia: T) =>
        recursivelyGetShortestPathFromSourceToVertex(pathVia, pathVia +: path)
      case _ =>
        path
    }
  }
}
