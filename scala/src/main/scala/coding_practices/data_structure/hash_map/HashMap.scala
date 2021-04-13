package coding_practices.data_structure.hash_map

import coding_practices.data_structure.doubly_linked_list.{DoublyLinkedList, DoublyLinkedListImpl, Node}

case class Key[K](key: K) {
  def getHashValue(lookupTableSize: Int): Int = {
    ((key.hashCode() % lookupTableSize) + lookupTableSize) % lookupTableSize
  }
}

case class Record[K, V](
  key: K,
  value: V,
  hashedKeyNode: Node[Int],
)

trait HashMap[K, V] {
  def put(key: K, value: V): HashMap[K, V]
  def get(key: K): Option[V]
  def remove(key: K): Option[V]
  def size: Int
  def toList: List[(K, V)]
}

class HashMapImpl[K, V]() extends HashMap[K, V] {

  val loadFactor: Double = 0.7
  var occupiedIndices: DoublyLinkedList[Int] = new DoublyLinkedListImpl[Int]()
  var lookupTable: Array[DoublyLinkedList[Record[K, V]]] = Array.fill[DoublyLinkedList[Record[K, V]]](1) {
    new DoublyLinkedListImpl[Record[K, V]]()
  }
  var currentSize: Int = 0

  implicit def toKey(key: K): Key[K] = Key(key = key)

  override def put(key: K, value: V): HashMap[K, V] = {
    val hashedKey: Int = key.getHashValue(lookupTable.length)
    val hashedKeyNode: Node[Int] = Node(hashedKey)
    val doublyLinkedList: DoublyLinkedList[Record[K, V]] = lookupTable(hashedKey)

    getNode(key) match {
      case Some(existingRecordNode: Node[Record[K, V]]) =>
        doublyLinkedList.removeNode(existingRecordNode)
        doublyLinkedList.pushBack(Record(key, value, existingRecordNode.value.hashedKeyNode))
      case _ =>
        currentSize = currentSize + 1
        if (doublyLinkedList.isEmpty) {
          occupiedIndices.pushBackNode(hashedKeyNode)
        }
        doublyLinkedList.pushBack(Record(key, value, hashedKeyNode))
    }

    if (currentSize.toDouble / lookupTable.length > loadFactor) resize()

    this
  }

  override def get(key: K): Option[V] = {
    val recordNodeOpt: Option[Node[Record[K, V]]] = getNode(key)
    val valueOpt: Option[V] = recordNodeOpt.map(_.value.value)

    valueOpt
  }

  private def getNode(key: K): Option[Node[Record[K, V]]] = {
    val hashedKey: Int = key.getHashValue(lookupTable.length)
    val doublyLinkedList: DoublyLinkedList[Record[K, V]] = lookupTable(hashedKey)
    val recordNodeOpt: Option[Node[Record[K, V]]] = doublyLinkedList.findNodeBy(_.key == key)

    recordNodeOpt
  }

  override def remove(key: K): Option[V] = {
    getNode(key).map { recordNode: Node[Record[K, V]] =>
      currentSize = currentSize - 1
      val hashedKeyNode: Node[Int] = recordNode.value.hashedKeyNode
      val hashedKey: Int = hashedKeyNode.value
      val doublyLinkedList: DoublyLinkedList[Record[K, V]] = lookupTable(hashedKey)
      doublyLinkedList.removeNode(recordNode)
      if (doublyLinkedList.isEmpty) {
        occupiedIndices.removeNode(hashedKeyNode)
      }

      recordNode.value.value
    }
  }

  private def resize(): Unit = {
    val currentOccupiedIndices: DoublyLinkedList[Int] = occupiedIndices
    val newOccupiedIndices: DoublyLinkedList[Int] = new DoublyLinkedListImpl[Int]()

    val currentLookupTable: Array[DoublyLinkedList[Record[K, V]]] = lookupTable
    val newLookupTable: Array[DoublyLinkedList[Record[K, V]]] =
      Array.fill[DoublyLinkedList[Record[K, V]]](lookupTable.length * 2) {
        new DoublyLinkedListImpl[Record[K, V]]()
      }
    lookupTable = newLookupTable

    currentOccupiedIndices.map { i: Int =>
      val doublyLinkedList: DoublyLinkedList[Record[K, V]] = currentLookupTable(i)
      doublyLinkedList.traverse { record: Record[K, V] =>
        val newHashedKey: Int = record.key.getHashValue(lookupTable.length)
        val newHashedKeyNode: Node[Int] = Node(newHashedKey)
        val newDoublyLinkedList: DoublyLinkedList[Record[K, V]] = newLookupTable(newHashedKey)

        if (newDoublyLinkedList.isEmpty) {
          newOccupiedIndices.pushBackNode(newHashedKeyNode)
        }
        newDoublyLinkedList.pushBack(Record(record.key, record.value, newHashedKeyNode))
      }
    }

    occupiedIndices = newOccupiedIndices
  }

  override def size: Int = currentSize

  override def toList: List[(K, V)] = {
    occupiedIndices.flatMap { occupiedIndex: Int =>
      lookupTable(occupiedIndex).map { record: Record[K, V] =>
        (record.key, record.value)
      }
    }.toList
  }
}
