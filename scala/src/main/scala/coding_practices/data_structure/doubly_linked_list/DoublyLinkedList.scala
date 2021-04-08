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
  def pushFront(value: T): DoublyLinkedList[T]
  def pushBack(value: T): DoublyLinkedList[T]
  def popFront(): DoublyLinkedList[T]
  def popBack(): DoublyLinkedList[T]
  def getFront: Option[T]
  def getBack: Option[T]
  def traverse[A](f: T => A): Unit
  def size: Int
}

class DoublyLinkedListImpl[T] extends DoublyLinkedList[T] {

  var head: Option[Node[T]] = None
  var tail: Option[Node[T]] = None
  var currentSize: Int = 0

  override def pushFront(value: T): DoublyLinkedList[T] = {
    val currentNode: Node[T] = Node(value = value)
    currentNode.setNextNode(head)
    head.foreach(_.setPreviousNode(Some(currentNode)))
    if (head.isEmpty) {
      tail = Some(currentNode)
    }
    head = Some(currentNode)
    currentSize = currentSize + 1
    this
  }

  override def pushBack(value: T): DoublyLinkedList[T] = {
    val currentNode: Node[T] = Node(value = value)
    currentNode.setPreviousNode(tail)
    tail.foreach(_.setNextNode(Some(currentNode)))
    if (tail.isEmpty) {
      head = Some(currentNode)
    }
    tail = Some(currentNode)
    currentSize = currentSize + 1
    this
  }

  override def popFront(): DoublyLinkedList[T] = {
    head match {
      case Some(headNode: Node[T]) =>
        val nextNodeOpt: Option[Node[T]] = headNode.getNextNode
        nextNodeOpt.foreach(_.setPreviousNode(None))
        head = nextNodeOpt
        currentSize = currentSize - 1
      case _ =>
    }

    if (head.isEmpty) {
      tail = None
    }

    this
  }

  override def popBack(): DoublyLinkedList[T] = {
    tail match {
      case Some(tailNode: Node[T]) =>
        val previousNodeOpt: Option[Node[T]] = tailNode.getPreviousNode
        previousNodeOpt.foreach(_.setNextNode(None))
        tail = previousNodeOpt
        currentSize = currentSize - 1
      case _ =>
    }

    if (tail.isEmpty) {
      head = None
    }

    this
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
}
