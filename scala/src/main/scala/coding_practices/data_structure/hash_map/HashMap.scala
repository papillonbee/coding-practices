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

  var lookupTable: Array[LinkedList[(K, V)]] = Array.fill[LinkedList[(K, V)]](100) {
    new LinkedListImpl[(K, V)]()
  }

  implicit def toKey(key: K): Key[K] = Key(key = key)

  override def put(key: K, value: V): HashMap[K, V] = {
    val hashedKey: Int = key.getHashValue(lookupTable.length)
    val linkedList: LinkedList[(K, V)] = lookupTable(hashedKey)
    linkedList.add((key, value))

    this
  }

  override def get(key: K): Option[V] = {
    val hashValue: Int = key.getHashValue(lookupTable.length)
    val linkedList: LinkedList[(K, V)] = lookupTable(hashValue)
    val keyValuePairOpt: Option[(K, V)] = linkedList.findBy(p => p._1 == key)
    val valueOpt: Option[V] = keyValuePairOpt.map(_._2)

    valueOpt
  }
}
