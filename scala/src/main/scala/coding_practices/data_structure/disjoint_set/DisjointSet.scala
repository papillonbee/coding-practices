package coding_practices.data_structure.disjoint_set

import coding_practices.data_structure.hash_map.{HashMap, HashMapImpl}

case class Node[T](
  value: T,
  private var parentValue: Option[T] = None,
  private var rank: Int = 0,
) {
  def getParentValue: Option[T] = parentValue

  def setParentValue(value: T): Unit = {
    parentValue = Some(value)
  }

  def getRank: Int = rank

  def incrementRank(): Unit = {
    rank = rank + 1
  }
}

trait DisjointSet[T] {
  def add(value: T): T
  def find(value: T): Option[T]
  def union(a: T, b: T): Boolean
}

class DisjointSetImpl[T](values: T*) extends DisjointSet[T] {

  val lookupTable: HashMap[T, Node[T]] = new HashMapImpl[T, Node[T]]()

  build()

  override def add(value: T): T = {
    val node: Node[T] = Node(value)
    lookupTable.put(value, node)
    value
  }

  override def find(value: T): Option[T] = findAbsoluteParentNode(value).map(_.value)

  private def findAbsoluteParentNode(value: T): Option[Node[T]] = {
    val nodeOpt: Option[Node[T]] = lookupTable.get(value)
    val absoluteParentNodeOpt: Option[Node[T]] = nodeOpt match {
      case Some(node: Node[T]) =>
        node.getParentValue match {
          case Some(parentValue: T) =>
            findAbsoluteParentNode(parentValue)
          case _ =>
            Some(node)
        }
      case _ =>
        None
    }

    (nodeOpt, absoluteParentNodeOpt) match {
      case (Some(node: Node[T]), Some(absoluteParentNode: Node[T])) if node != absoluteParentNode =>
        node.setParentValue(absoluteParentNode.value)
      case _ =>
    }

    absoluteParentNodeOpt
  }

  override def union(a: T, b: T): Boolean = {
    if (a == b) {
      throw new IllegalArgumentException("Cannot union two same values")
    } else {
      val absoluteParentNodeOfAOpt: Option[Node[T]] = findAbsoluteParentNode(a)
      val absoluteParentNodeOfBOpt: Option[Node[T]] = findAbsoluteParentNode(b)

      val isCyclic: Boolean = (absoluteParentNodeOfAOpt, absoluteParentNodeOfBOpt) match {
        case (Some(absoluteParentNodeOfA: Node[T]), Some(absoluteParentNodeOfB: Node[T]))
          if absoluteParentNodeOfA != absoluteParentNodeOfB =>

          if (absoluteParentNodeOfA.getRank > absoluteParentNodeOfB.getRank) {
            absoluteParentNodeOfB.setParentValue(absoluteParentNodeOfA.value)
          } else if (absoluteParentNodeOfB.getRank > absoluteParentNodeOfA.getRank) {
            absoluteParentNodeOfA.setParentValue(absoluteParentNodeOfB.value)
          } else {
            absoluteParentNodeOfA.setParentValue(absoluteParentNodeOfB.value)
            absoluteParentNodeOfB.incrementRank()
          }
          false
        case _ =>
          true
      }

      !isCyclic
    }
  }

  private def build(): Unit = values.foreach(add)
}
