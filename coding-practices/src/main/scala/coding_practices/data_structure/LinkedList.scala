package coding_practices.data_structure

object LinkedList {
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
    def traverse[A](f: Node[T] => A): Unit
  }

  class LinkedListImpl[T]() extends LinkedList[T] {
    override def add(value: T): LinkedList[T] = {
      val currentNode: Node[T] = Node(value = value)
      currentNode.setNextNode(head)
      head = Some(currentNode)
      this
    }

    override def traverse[A](f: Node[T] => A): Unit = head.foreach(recursivelyTraverse(_)(f))

    private def recursivelyTraverse[A](node: Node[T])(f: Node[T] => A): Unit = {
      f(node)
      node.getNextNode.foreach(recursivelyTraverse(_)(f))
    }
  }
}
