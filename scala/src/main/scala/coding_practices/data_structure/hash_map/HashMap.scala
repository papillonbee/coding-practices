package coding_practices.data_structure.hash_map

import coding_practices.data_structure.linked_list.{LinkedList, LinkedListImpl}

case class Key[K](key: K) {
  def getHashValue(lookupTableSize: Int): Int = {
    ((key.hashCode() % lookupTableSize) + lookupTableSize) % lookupTableSize
  }
}

trait HashMap[K, V] {
  def put(key: K, value: V): HashMap[K, V]
  def get(key: K): Option[V]
}

class HashMapImpl[K, V]() extends HashMap[K, V] {

  val loadFactor: Double = 0.7
  var occupiedIndices: LinkedList[Int] = new LinkedListImpl[Int]()
  var lookupTable: Array[LinkedList[(K, V)]] = Array.fill[LinkedList[(K, V)]](1) {
    new LinkedListImpl[(K, V)]()
  }

  implicit def toKey(key: K): Key[K] = Key(key = key)

  override def put(key: K, value: V): HashMap[K, V] = {
    val hashedKey: Int = key.getHashValue(lookupTable.length)
    val linkedList: LinkedList[(K, V)] = lookupTable(hashedKey)

    if (linkedList.isEmpty) {
      occupiedIndices.add(hashedKey)
    }
    linkedList.add((key, value))

    if (occupiedIndices.size.toDouble / lookupTable.length > loadFactor) resize()

    this
  }

  override def get(key: K): Option[V] = {
    val hashedKey: Int = key.getHashValue(lookupTable.length)
    val linkedList: LinkedList[(K, V)] = lookupTable(hashedKey)
    val keyValuePairOpt: Option[(K, V)] = linkedList.findBy(_._1 == key)
    val valueOpt: Option[V] = keyValuePairOpt.map(_._2)

    valueOpt
  }

  private def resize(): Unit = {
    val currentOccupiedIndices: LinkedList[Int] = occupiedIndices
    val newOccupiedIndices: LinkedList[Int] = new LinkedListImpl[Int]()

    val currentLookupTable: Array[LinkedList[(K, V)]] = lookupTable
    val newLookupTable: Array[LinkedList[(K, V)]] = Array.fill[LinkedList[(K, V)]](lookupTable.length * 2) {
      new LinkedListImpl[(K, V)]()
    }
    lookupTable = newLookupTable

    currentOccupiedIndices.map { i: Int =>
      val linkedList: LinkedList[(K, V)] = currentLookupTable(i)
      linkedList.traverse { keyValuePair: (K, V) =>
        val newHashedKey: Int = keyValuePair._1.getHashValue(lookupTable.length)
        val newLinkedList: LinkedList[(K, V)] = newLookupTable(newHashedKey)

        if (newLinkedList.isEmpty) {
          newOccupiedIndices.add(newHashedKey)
        }
        newLinkedList.add(keyValuePair)
      }
    }

    occupiedIndices = newOccupiedIndices
  }
}
