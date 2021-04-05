import scala.io.StdIn.readLine
import scala.io.{BufferedSource, Source}

package object LinkedList {
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
    def find(value: T): Option[Node[T]]
    def remove(value: T): LinkedList[T]
    def traverse[A](f: Node[T] => A): Unit
  }

  class LinkedListImpl[T]() extends LinkedList[T] {
    override def add(value: T): LinkedList[T] = {
      val currentNode: Node[T] = Node(value = value)
      currentNode.setNextNode(head)
      head = Some(currentNode)
      this
    }

    override def find(value: T): Option[Node[T]] = head.flatMap(find(value, _))

    private def find(value: T, node: Node[T]): Option[Node[T]] = {
      if (value == node.value) {
        Some(node)
      } else {
        node.getNextNode.flatMap(find(value, _))
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

    override def traverse[A](f: Node[T] => A): Unit = head.foreach(recursivelyTraverse(_)(f))

    private def recursivelyTraverse[A](node: Node[T])(f: Node[T] => A): Unit = {
      f(node)
      node.getNextNode.foreach(recursivelyTraverse(_)(f))
    }
  }
}

object LinkedListMain {
  import LinkedList.{LinkedList, LinkedListImpl}

  def main(args: Array[String]): Unit = {
    val bufferedSource: BufferedSource = Source.fromFile("src/main/resources/node_inputs.txt")
    val inputs: Seq[String] = bufferedSource.getLines().toSeq
    val inputs: Seq[String] = Iterator.continually(readLine()).takeWhile(_.nonEmpty).toSeq
    bufferedSource.close()

    val inputsInt: Seq[Int] = inputs.map(_.toInt)

    val linkedList: LinkedList[Int] = new LinkedListImpl[Int]()

    inputsInt.map(input => linkedList.add(input))

    linkedList.traverse(println)

    println()

    linkedList.remove(2)

    linkedList.traverse(println)

    println()

    linkedList.remove(999)

    linkedList.traverse(println)

    println()

    linkedList.remove(55)

    linkedList.traverse(println)

    println()

    linkedList.remove(5)

    linkedList.traverse(println)

    println()
  }
}
