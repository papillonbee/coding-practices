package coding_practices.data_structure.doubly_linked_list

import scala.annotation.tailrec

case class Node[T](
  value: T,
  private var previousNode: Option[Node[T]] = None,
  private var nextNode: Option[Node[T]] = None,
) {
  def getPreviousNode: Option[Node[T]] = previousNode

  def getNextNode: Option[Node[T]] = nextNode

  def setPreviousNode(node: Option[Node[T]]): Node[T] = {
    previousNode = node
    this
  }

  def setNextNode(node: Option[Node[T]]): Node[T] = {
    nextNode = node
    this
  }
}

trait DoublyLinkedList[T] {
  def pushFront(value: T): T
  def pushBack(value: T): T
  def popFront(): Option[T]
  def popBack(): Option[T]
  def getFront: Option[T]
  def getBack: Option[T]
  def traverse[A](f: T => A): Unit
  def size: Int

  def pushFrontNode(node: Node[T]): Node[T]
  def pushBackNode(node: Node[T]): Node[T]
  def removeNode(node: Node[T]): Option[Node[T]]
  def getFrontNode: Option[Node[T]]
  def getBackNode: Option[Node[T]]
}

class DoublyLinkedListImpl[T] extends DoublyLinkedList[T] {

  var head: Option[Node[T]] = None
  var tail: Option[Node[T]] = None
  var currentSize: Int = 0

  override def pushFront(value: T): T = {
    val node: Node[T] = Node(value = value)
    val pushedFrontNode: Node[T] = pushFrontNode(node)
    pushedFrontNode.value
  }

  override def pushBack(value: T): T = {
    val node: Node[T] = Node(value = value)
    val pushedBackNode: Node[T] = pushBackNode(node)
    pushedBackNode.value
  }

  override def popFront(): Option[T] = {
    head match {
      case Some(headNode: Node[T]) =>
        val nextNodeOpt: Option[Node[T]] = headNode.getNextNode
        nextNodeOpt.foreach(_.setPreviousNode(None))
        head = nextNodeOpt
        currentSize = currentSize - 1

        if (head.isEmpty) {
          tail = None
        }

        Some(headNode.value)
      case _ =>
        None
    }
  }

  override def popBack(): Option[T] = {
    tail match {
      case Some(tailNode: Node[T]) =>
        val previousNodeOpt: Option[Node[T]] = tailNode.getPreviousNode
        previousNodeOpt.foreach(_.setNextNode(None))
        tail = previousNodeOpt
        currentSize = currentSize - 1

        if (tail.isEmpty) {
          head = None
        }

        Some(tailNode.value)
      case _ =>
        None
    }
  }

  override def getFront: Option[T] = head.map(_.value)

  override def getBack: Option[T] = tail.map(_.value)

  override def traverse[A](f: T => A): Unit = recursivelyTraverse(head)(f)

  @tailrec
  private def recursivelyTraverse[A](nodeOpt: Option[Node[T]])(f: T => A): Unit = {
    nodeOpt match {
      case Some(node: Node[T]) =>
        f(node.value)
        recursivelyTraverse(node.getNextNode)(f)
      case _ =>
    }
  }

  override def size: Int = currentSize

  override def pushFrontNode(node: Node[T]): Node[T] = {
    node.setNextNode(head)
    head.foreach(_.setPreviousNode(Some(node)))
    if (head.isEmpty) {
      tail = Some(node)
    }
    head = Some(node)
    currentSize = currentSize + 1
    node
  }

  override def pushBackNode(node: Node[T]): Node[T] = {
    node.setPreviousNode(tail)
    tail.foreach(_.setNextNode(Some(node)))
    if (tail.isEmpty) {
      head = Some(node)
    }
    tail = Some(node)
    currentSize = currentSize + 1
    node
  }

  override def removeNode(node: Node[T]): Option[Node[T]] = {
    val removedNodeOpt: Option[Node[T]] = (node.getPreviousNode, node.getNextNode, head, tail) match {
      case (Some(previousNode: Node[T]), Some(nextNode: Node[T]), _, _) =>
        previousNode.setNextNode(Some(nextNode))
        nextNode.setPreviousNode(Some(previousNode))
        currentSize = currentSize - 1
        Some(node)
      case (None, Some(nextNode: Node[T]), Some(headNode: Node[T]), _) if headNode == node =>
        nextNode.setPreviousNode(None)
        head = Some(nextNode)
        currentSize = currentSize - 1
        Some(node)
      case (Some(previousNode: Node[T]), None, _, Some(tailNode: Node[T])) if tailNode == node =>
        previousNode.setNextNode(None)
        tail = Some(previousNode)
        Some(node)
      case (None, None, Some(headNode: Node[T]), Some(tailNode: Node[T])) if headNode == node && tailNode == node =>
        head = None
        tail = None
        currentSize = currentSize - 1
        Some(node)
      case _ =>
        None
    }
    removedNodeOpt.map(_.setNextNode(None)).map(_.setPreviousNode(None))
  }

  override def getFrontNode: Option[Node[T]] = head

  override def getBackNode: Option[Node[T]] = tail
}
