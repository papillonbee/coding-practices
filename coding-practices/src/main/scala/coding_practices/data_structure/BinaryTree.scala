package coding_practices.data_structure

object BinaryTree {
  case class Node[T](
    value: T,
    private var leftNode: Option[Node[T]] = None,
    private var rightNode: Option[Node[T]] = None,
  ) {
    def getLeftNode: Option[Node[T]] = leftNode

    def getRightNode: Option[Node[T]] = rightNode

    def setLeftNode(node: Option[Node[T]]): Node[T] = {
      leftNode = node
      this
    }

    def setRightNode(node: Option[Node[T]]): Node[T] = {
      rightNode = node
      this
    }
  }

  trait BinaryTree[T] {
    protected var root: Option[Node[T]] = None

    def add(value: T): BinaryTree[T]
    def traverse[A](f: Node[T] => A): Unit
  }

  class BinaryTreeImpl[T]()(
    implicit ord: Ordering[T]
  ) extends BinaryTree[T] {
    import ord.mkOrderingOps

    override def add(value: T): BinaryTree[T] = {
      val currentNode: Node[T] = Node(value = value)
      root.map { node: Node[T] =>
        currentNode.value < node.value
      }
      this
    }

    override def traverse[A](f: Node[T] => A): Unit = {

    }
  }
}
