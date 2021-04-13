package coding_practices.data_structure.segment_tree

case class Range(
  sum: Int,
  min: Int,
)

object Range {
  val empty: Range = Range(
    sum = 0,
    min = Int.MaxValue,
  )
}

case class Node(
  range: Range,
  fromIndex: Int,
  toIndex: Int,
) {
  def isTotallyOverlapping(queryFromIndex: Int, queryToIndex: Int): Boolean = {
    queryFromIndex <= fromIndex && toIndex <= queryToIndex
  }
  def isNonOverlapping(queryFromIndex: Int, queryToIndex: Int): Boolean = {
    queryFromIndex > toIndex || queryToIndex < fromIndex
  }
  def contains(index: Int): Boolean = {
    fromIndex <= index && index <= toIndex
  }
  def isLeafNode: Boolean = fromIndex == toIndex
}

trait SegmentTree {
  def getRange(fromIndex: Int, toIndex: Int): Range
  def update(value: Int, atIndex: Int): Unit
}

class SegmentTreeImpl(values: Array[Int]) extends SegmentTree {

  val segmentTree: Array[Option[Node]] = Array.fill(2 * values.length - 1)(None)

  build()

  override def getRange(fromIndex: Int, toIndex: Int): Range = recursivelyGetRange(
    fromIndex = fromIndex,
    toIndex = toIndex,
  )

  private def recursivelyGetRange(
    fromIndex: Int,
    toIndex: Int,
    segmentTreeIndex: Int = 0,
  ): Range = {
    segmentTree(segmentTreeIndex) match {
      case Some(node: Node) =>
        if (node.isNonOverlapping(fromIndex, toIndex)) {
          Range.empty
        } else if (node.isTotallyOverlapping(fromIndex, toIndex)) {
          node.range
        } else {
          val leftRange: Range = recursivelyGetRange(fromIndex, toIndex, 2 * segmentTreeIndex + 1)
          val rightRange: Range = recursivelyGetRange(fromIndex, toIndex, 2 * segmentTreeIndex + 2)
          Range(
            sum = leftRange.sum + rightRange.sum,
            min = math.min(leftRange.min, rightRange.min),
          )
        }
      case _ =>
        Range.empty
    }
  }

  override def update(value: Int, atIndex: Int): Unit = {
    val incrementalValueChange: Int = value - values(atIndex)
    values(atIndex) = value
    recursivelyUpdate(value, incrementalValueChange, atIndex)
  }

  private def recursivelyUpdate(
    value: Int,
    incrementalValueChange: Int,
    atIndex: Int,
    segmentTreeIndex: Int = 0,
  ): Unit = {
    segmentTree(segmentTreeIndex) match {
      case Some(node: Node) if node.contains(atIndex) =>
        val updatedNode: Node = node.copy(
          range = Range(
            sum = node.range.sum + incrementalValueChange,
            min = math.min(node.range.min, value),
          )
        )
        segmentTree(segmentTreeIndex) = Some(updatedNode)
        if (!node.isLeafNode) {
          recursivelyUpdate(value, incrementalValueChange, atIndex, 2 * segmentTreeIndex + 1)
          recursivelyUpdate(value, incrementalValueChange, atIndex, 2 * segmentTreeIndex + 2)
        }
      case _ =>
    }
  }

  private def build(fromIndex: Int = 0, toIndex: Int = values.length - 1, segmentTreeIndex: Int = 0): Range = {
    val range: Range = if (toIndex > fromIndex) {
      val m: Int = (fromIndex + toIndex) / 2
      val leftValue: Range = build(fromIndex, m, 2 * segmentTreeIndex + 1)
      val rightValue: Range = build(m + 1, toIndex, 2 * segmentTreeIndex + 2)

      Range(
        sum = leftValue.sum + rightValue.sum,
        min = math.min(leftValue.min, rightValue.min),
      )
    } else {
      val leafValue: Int = values(fromIndex)

      Range(
        sum = leafValue,
        min = leafValue,
      )
    }

    segmentTree(segmentTreeIndex) = Some(Node(range, fromIndex, toIndex))
    range
  }
}
