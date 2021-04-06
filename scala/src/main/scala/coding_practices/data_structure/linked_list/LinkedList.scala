package coding_practices.data_structure.linked_list

case class Node[T](
  value: T,
  private var nextNode: Option[Node[T]] = None,
) {
  def getNextNode: Option[Node[T]] = nextNode

  def setNextNode(node: Option[Node[T]]): Node[T] = {
    nextNode = node
    this
  }
}

trait LinkedList[T] {
  protected var head: Option[Node[T]] = None

  def add(value: T): LinkedList[T]

  def find(value: T): Option[T]

  def findBy(f: T => Boolean): Option[T]

  def remove(value: T): LinkedList[T]

  def map[R](f: T => R): LinkedList[R]

  def traverse[A](f: Node[T] => A): Unit
}

class LinkedListImpl[T]() extends LinkedList[T] {
  override def add(value: T): LinkedList[T] = {
    val currentNode: Node[T] = Node(value = value)
    currentNode.setNextNode(head)
    head = Some(currentNode)
    this
  }

  override def find(value: T): Option[T] = findNode(value).map(_.value)

  private def findNode(value: T): Option[Node[T]] = head.flatMap(findNode(value, _))

  private def findNode(value: T, node: Node[T]): Option[Node[T]] = {
    if (value == node.value) {
      Some(node)
    } else {
      node.getNextNode.flatMap(findNode(value, _))
    }
  }

  override def findBy(f: T => Boolean): Option[T] = findNodeBy(f).map(_.value)

  private def findNodeBy(f: T => Boolean): Option[Node[T]] = head.flatMap(findNodeBy(f, _))

  private def findNodeBy(f: T => Boolean, node: Node[T]): Option[Node[T]] = {
    if (f(node.value)) {
      Some(node)
    } else {
      node.getNextNode.flatMap(findNodeBy(f, _))
    }
  }

  override def remove(value: T): LinkedList[T] = {
    head match {
      case Some(headNode) =>
        if (value == headNode.value) {
          head = headNode.getNextNode
        } else {
          remove(value, headNode)
        }
    }
    this
  }

  private def remove(value: T, node: Node[T]): Unit = {
    node.getNextNode match {
      case Some(nextNode) if value == nextNode.value =>
        node.setNextNode(nextNode.getNextNode)
      case _ =>
        node.getNextNode.foreach(remove(value, _))
    }
  }

  override def map[R](f: T => R): LinkedList[R] = {
    val emptyLinkedList: LinkedList[R] = new LinkedListImpl[R]()

    head.map(recursivelyMap(_, emptyLinkedList)(f)).getOrElse(emptyLinkedList)
  }

  private def recursivelyMap[R](node: Node[T], linkedList: LinkedList[R])(f: T => R): LinkedList[R] = {
    val updatedLinkedList: LinkedList[R] = linkedList.add(f(node.value))
    node.getNextNode match {
      case Some(nextNode) =>
        recursivelyMap(nextNode, updatedLinkedList)(f)
      case _ =>
        updatedLinkedList
    }
  }

  override def traverse[A](f: Node[T] => A): Unit = head.foreach(recursivelyTraverse(_)(f))

  private def recursivelyTraverse[A](node: Node[T])(f: Node[T] => A): Unit = {
    f(node)
    node.getNextNode.foreach(recursivelyTraverse(_)(f))
  }
}
