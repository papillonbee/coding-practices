package coding_practices.data_structure.segment_tree

import org.scalatest.{OneInstancePerTest, OptionValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SegmentTreeTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues {

  describe("SegmentTree") {
    val values: Array[Int] = Array(1, 8, 7, 1, 2, 9, 2, 6)
    val segmentTree: SegmentTree = new SegmentTreeImpl(values)

    it("should get range sum and range min from fromIndex to toIndex correctly") {
      val fromIndex: Int = 2
      val toIndex: Int = 7
      val range: Range = segmentTree.getRange(fromIndex, toIndex)
      range.sum shouldEqual values.slice(fromIndex, toIndex + 1).sum
      range.min shouldEqual values.slice(fromIndex, toIndex + 1).min
    }

    it("should update value at atIndex correctly") {
      val value: Int = -10
      val atIndex: Int = 2
      segmentTree.update(value, atIndex)
      values(atIndex) = value

      val fromIndex: Int = 1
      val toIndex: Int = 3
      val range: Range = segmentTree.getRange(fromIndex, toIndex)
      range.sum shouldEqual values.slice(fromIndex, toIndex + 1).sum
      range.min shouldEqual values.slice(fromIndex, toIndex + 1).min
    }
  }
}
