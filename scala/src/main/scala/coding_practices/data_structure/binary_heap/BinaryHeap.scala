package coding_practices.data_structure.binary_heap

import coding_practices.data_structure.hash_map.{HashMap, HashMapImpl}

import scala.annotation.tailrec

case class Node[T](
  value: T,
  var index: Int,
) {
  def getLeftChildIndex: Int = 2 * index + 1
  def getRightChildIndex: Int = 2 * index + 2
  def getParentIndex: Int = (index - 1) / 2
}

trait BinaryHeap[T] {
  def push(value: T): T
  def pop(): Option[T]
  def peek: Option[T]
  def size: Int
}

class BinaryHeapImpl[T](values: T*)(
  implicit ordering: Ordering[T]
) extends BinaryHeap[T] {

  import ordering.mkOrderingOps

  val memoryTable: HashMap[Int, Node[T]] = new HashMapImpl[Int, Node[T]]()
  var lastIndex: Option[Int] = None

  build()

  override def push(value: T): T = {
    incrementIndex()
    lastIndex match {
      case Some(index: Int) =>
        val node: Node[T] = Node(value = value, index = index)
        memoryTable.put(index, node)
        heapifyUp(index)
      case _ =>
    }

    value
  }

  override def pop(): Option[T] = {
    lastIndex match {
      case Some(index: Int) if index > 0 =>
        val rootNodeOpt: Option[Node[T]] = memoryTable.get(0)
        val removedLastNodeOpt: Option[Node[T]] = memoryTable.remove(index)
        (rootNodeOpt, removedLastNodeOpt) match {
          case (Some(rootNode: Node[T]), Some(removedLastNode: Node[T])) =>
            removedLastNode.index = 0
            memoryTable.put(0, removedLastNode)
            decrementIndex()
            heapifyDown(0)
            Some(rootNode.value)
          case _ =>
            None
        }
      case Some(0) =>
        val removedHeadNodeOpt: Option[Node[T]] = memoryTable.remove(0)
        removedHeadNodeOpt match {
          case Some(removedHeadNode: Node[T]) =>
            decrementIndex()
            Some(removedHeadNode.value)
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  override def peek: Option[T] = memoryTable.get(0).map(_.value)

  override def size: Int = lastIndex.map(_ + 1).getOrElse(0)

  private def incrementIndex(): Unit = {
    val incrementedIndex: Option[Int] = lastIndex match {
      case Some(index: Int) => Some(index + 1)
      case _ => Some(0)
    }
    lastIndex = incrementedIndex
  }

  private def decrementIndex(): Unit = {
    val decrementedIndex: Option[Int] = lastIndex match {
      case Some(index: Int) if index > 0 => Some(index - 1)
      case _ => None
    }
    lastIndex = decrementedIndex
  }

  private def build(): Unit = {
    values.zipWithIndex.foreach { case (value: T, index: Int) =>
      incrementIndex()
      val node: Node[T] = Node(value = value, index = index)
      memoryTable.put(index, node)
    }

    getLastInternalNodeIndex match {
      case Some(lastInternalNodeIndex: Int) =>
        (lastInternalNodeIndex to 0 by -1).foreach(heapifyDown)
      case _ =>
    }
  }

  private def getLastInternalNodeIndex: Option[Int] = {
    lastIndex match {
      case Some(i) if i > 0 =>
        Some((i - 1) / 2)
      case _ =>
        None
    }
  }

  @tailrec
  private def heapifyUp(index: Int): Unit = {
    val nodeOpt: Option[Node[T]] = memoryTable.get(index)
    val parentNodeOpt: Option[Node[T]] = nodeOpt.map(_.getParentIndex).flatMap(memoryTable.get)

    val swappingNodeOpt: Option[Node[T]] = (nodeOpt, parentNodeOpt) match {
      case (Some(node: Node[T]), Some(parentNode: Node[T])) if node.value > parentNode.value =>
        Some(parentNode)
      case _ =>
        None
    }

    (swappingNodeOpt, nodeOpt) match {
      case (Some(swappingNode: Node[T]), Some(node: Node[T])) =>
        val swappingNodeIndex: Int = swappingNode.index
        val nodeIndex: Int = node.index
        swappingNode.index = nodeIndex
        node.index = swappingNodeIndex
        memoryTable.put(swappingNode.index, swappingNode)
        memoryTable.put(node.index, node)
        heapifyUp(node.index)
      case _ =>
    }
  }

  @tailrec
  private def heapifyDown(index: Int): Unit = {
    val nodeOpt: Option[Node[T]] = memoryTable.get(index)
    val leftChildNodeOpt: Option[Node[T]] = nodeOpt.map(_.getLeftChildIndex).flatMap(memoryTable.get)
    val rightChildNodeOpt: Option[Node[T]] = nodeOpt.map(_.getRightChildIndex).flatMap(memoryTable.get)

    val swappingNodeOpt: Option[Node[T]] = (nodeOpt, leftChildNodeOpt, rightChildNodeOpt) match {
      case (Some(node: Node[T]), Some(leftChildNode: Node[T]), Some(rightChildNode: Node[T])) =>
        if (leftChildNode.value > node.value && leftChildNode.value >= rightChildNode.value) {
          Some(leftChildNode)
        } else if (rightChildNode.value > node.value && rightChildNode.value >= leftChildNode.value) {
          Some(rightChildNode)
        } else {
          None
        }
      case (Some(node: Node[T]), Some(leftChildNode: Node[T]), _) if leftChildNode.value > node.value =>
        Some(leftChildNode)
      case (Some(node: Node[T]), _, Some(rightChildNode: Node[T])) if rightChildNode.value > node.value =>
        Some(rightChildNode)
      case _ =>
        None
    }

    (swappingNodeOpt, nodeOpt) match {
      case (Some(swappingNode: Node[T]), Some(node: Node[T])) =>
        val swappingNodeIndex: Int = swappingNode.index
        val nodeIndex: Int = node.index
        swappingNode.index = nodeIndex
        node.index = swappingNodeIndex
        memoryTable.put(swappingNode.index, swappingNode)
        memoryTable.put(node.index, node)
        heapifyDown(node.index)
      case _ =>
    }
  }
}
