package coding_practices.data_structure.binary_tree

case class Node[T](
  var value: T,
  private var leftNode: Option[Node[T]] = None,
  private var rightNode: Option[Node[T]] = None,
) {
  def isLeafNode: Boolean = getNumberOfChildren == 0

  def hasOneChild: Boolean = getNumberOfChildren == 1

  def hasTwoChildren: Boolean = getNumberOfChildren == 2

  private def getNumberOfChildren: Int = {
    (leftNode.isEmpty, rightNode.isEmpty) match {
      case (true, true) => 0
      case (false, false) => 2
      case _ => 1
    }
  }

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

  def find(value: T): Option[Node[T]]

  def remove(value: T): BinaryTree[T]

  def traverse[A](f: Node[T] => A): Unit
}

class BinaryTreeImpl[T]()(
  implicit ord: Ordering[T]
) extends BinaryTree[T] {

  import ord.mkOrderingOps

  override def add(value: T): BinaryTree[T] = {
    val currentNode: Node[T] = Node(value = value)
    if (root.isEmpty) {
      root = Some(currentNode)
    } else {
      root.foreach(add(currentNode, _))
    }
    this
  }

  private def add(node: Node[T], currentRoot: Node[T]): Node[T] = {
    if (node.value < currentRoot.value) {
      currentRoot.getLeftNode match {
        case Some(leftNode: Node[T]) =>
          add(node, leftNode)
        case None =>
          val newLeftNode: Node[T] = currentRoot.setLeftNode(Some(node))
          newLeftNode
      }
    } else {
      currentRoot.getRightNode match {
        case Some(rightNode: Node[T]) =>
          add(node, rightNode)
        case None =>
          val newRightNode: Node[T] = currentRoot.setRightNode(Some(node))
          newRightNode
      }
    }
  }

  override def find(value: T): Option[Node[T]] = {
    root.flatMap(find(value, _))
  }

  private def find(value: T, node: Node[T]): Option[Node[T]] = {
    if (value == node.value) {
      Some(node)
    } else {
      node.getLeftNode.flatMap(find(value, _)).orElse(node.getRightNode.flatMap(find(value, _)))
    }
  }

  private def findMax(node: Node[T]): Node[T] = {
    node.getRightNode.map(findMax).getOrElse(node)
  }

  override def remove(value: T): BinaryTree[T] = {
    root match {
      case Some(rootNode: Node[T]) =>
        if (value == rootNode.value) {
          if (rootNode.isLeafNode) {
            root = None
          } else if (rootNode.hasOneChild) {
            root = rootNode.getLeftNode.orElse(rootNode.getRightNode)
          } else {
            val rightMostOfLeftNodeOpt: Option[Node[T]] = rootNode.getLeftNode.map(findMax)
            rightMostOfLeftNodeOpt.foreach { rightMostOfLeftNode: Node[T] =>
              rootNode.value = rightMostOfLeftNode.value
              remove(rightMostOfLeftNode.value, rootNode)
            }
          }
        } else {
          remove(value, rootNode)
        }
    }
    this
  }

  private def remove(value: T, node: Node[T]): Unit = {
    (node.getLeftNode, node.getRightNode) match {
      case (Some(leftNode), _) if value == leftNode.value =>
        if (leftNode.isLeafNode) {
          node.setLeftNode(None)
        } else if (leftNode.hasOneChild) {
          node.setLeftNode(leftNode.getLeftNode.orElse(leftNode.getRightNode))
        } else {
          val rightMostOfLeftOfLeftNodeOpt: Option[Node[T]] = leftNode.getLeftNode.map(findMax)
          rightMostOfLeftOfLeftNodeOpt.foreach { rightMostOfLeftOfLeftNode: Node[T] =>
            leftNode.value = rightMostOfLeftOfLeftNode.value
            remove(rightMostOfLeftOfLeftNode.value, leftNode)
          }
        }
      case (_, Some(rightNode)) if value == rightNode.value =>
        if (rightNode.isLeafNode) {
          node.setRightNode(None)
        } else if (rightNode.hasOneChild) {
          node.setRightNode(rightNode.getLeftNode.orElse(rightNode.getRightNode))
        } else {
          val rightMostOfRightOfLeftNodeOpt: Option[Node[T]] = rightNode.getLeftNode.map(findMax)
          rightMostOfRightOfLeftNodeOpt.foreach { rightMostOfRightOfLeftNode: Node[T] =>
            rightNode.value = rightMostOfRightOfLeftNode.value
            remove(rightMostOfRightOfLeftNode.value, rightNode)
          }
        }
      case _ =>
        node.getLeftNode.foreach(remove(value, _))
        node.getRightNode.foreach(remove(value, _))
    }
  }

  override def traverse[A](f: Node[T] => A): Unit = root.foreach(recursivelyTraverse(_)(f))

  private def recursivelyTraverse[A](node: Node[T])(f: Node[T] => A): Unit = {
    f(node)
    node.getLeftNode.foreach(recursivelyTraverse(_)(f))
    node.getRightNode.foreach(recursivelyTraverse(_)(f))
  }
}

