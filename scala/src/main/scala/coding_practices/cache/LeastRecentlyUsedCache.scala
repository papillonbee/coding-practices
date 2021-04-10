package coding_practices.cache

import coding_practices.data_structure.doubly_linked_list.{DoublyLinkedList, DoublyLinkedListImpl, Node}
import coding_practices.data_structure.hash_map.{HashMap, HashMapImpl}

trait LeastRecentlyUsedCache[K, V] {
  def put(key: K, value: V): Unit
  def get(key: K, getValue: () => Option[V]): Option[V]
}

class LeastRecentlyUsedCacheImpl[K, V](
  maximumSize: Int,
) extends LeastRecentlyUsedCache[K, V] {

  val memoryTable: HashMap[K, Node[(K, V)]] = new HashMapImpl[K, Node[(K, V)]]()
  val leastRecentlyUsedQueue: DoublyLinkedList[(K, V)] = new DoublyLinkedListImpl[(K, V)]()

  override def put(key: K, value: V): Unit = {
    memoryTable.get(key) match {
      case Some(foundNode: Node[(K, V)]) =>
        leastRecentlyUsedQueue.removeNode(foundNode)
      case _ =>
    }

    val node: Node[(K, V)] = Node(value = (key, value))
    val pushedBackNode: Node[(K, V)] = leastRecentlyUsedQueue.pushBackNode(node)
    memoryTable.put(key, pushedBackNode)

    if (memoryTable.size > maximumSize) evict()
  }

  override def get(key: K, getValue: () => Option[V]): Option[V] = {
    memoryTable.get(key) match {
      case Some(foundNode: Node[(K, V)]) =>
        val removedNodeOpt: Option[Node[(K, V)]] = leastRecentlyUsedQueue.removeNode(foundNode)
        val updatedNodeOpt: Option[Node[(K, V)]] = removedNodeOpt.map(leastRecentlyUsedQueue.pushBackNode)
        updatedNodeOpt match {
          case Some(updatedNode: Node[(K, V)]) =>
            memoryTable.put(key, updatedNode)
            val value: V = updatedNode.value._2
            Some(value)
          case _ =>
            None
        }
      case _ =>
        getValue() match {
          case Some(value: V) =>
            put(key, value)
            Some(value)
          case _ =>
            None
        }
    }
  }

  private def evict(): Unit = {
    leastRecentlyUsedQueue.getFront match {
      case Some(leastRecentlyUsedKeyValuePair: (K, V)) =>
        memoryTable.remove(leastRecentlyUsedKeyValuePair._1)
        leastRecentlyUsedQueue.popFront()
      case _ =>
    }
  }
}
