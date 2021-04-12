package coding_practices.data_structure.disjoint_set

import coding_practices.model.Dinosaur
import org.scalatest.{OneInstancePerTest, OptionValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DisjointSetTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues {

  describe("DisjointSet") {
    val dinosaurA: Dinosaur = Dinosaur("A", 1)
    val dinosaurB: Dinosaur = Dinosaur("B", 2)
    val dinosaurC: Dinosaur = Dinosaur("C", 3)
    val dinosaurD: Dinosaur = Dinosaur("D", 4)
    val dinosaurE: Dinosaur = Dinosaur("E", 5)

    val disjointSet: DisjointSet[Dinosaur] = new DisjointSetImpl[Dinosaur](
      dinosaurA, dinosaurB, dinosaurC, dinosaurD, dinosaurE
    )

    it("should find its absolute parent correctly") {
      disjointSet.union(dinosaurA, dinosaurB)
      disjointSet.union(dinosaurC, dinosaurD)
      disjointSet.union(dinosaurA, dinosaurC)

      disjointSet.find(dinosaurB).value shouldEqual disjointSet.find(dinosaurD).value
      disjointSet.find(dinosaurE).value shouldEqual dinosaurE
    }

    it("should throw illegal argument exception if union is done on the same two values") {
      the [IllegalArgumentException] thrownBy
        disjointSet.union(dinosaurA, dinosaurA) should have message "Cannot union two same values"
    }

    it("should detect cyclic path in a graph") {
      val edgeList: Seq[(Dinosaur, Dinosaur)] = Seq(
        (dinosaurA, dinosaurB),
        (dinosaurB, dinosaurC),
        (dinosaurC, dinosaurD),
        (dinosaurD, dinosaurE),
        (dinosaurE, dinosaurA),
      )

      val mergedEdgeList: Seq[(Dinosaur, Dinosaur)] = edgeList.takeWhile { case (x: Dinosaur, y: Dinosaur) =>
        disjointSet.union(x, y)
      }

      val notMergedEdgeOpt: Option[(Dinosaur, Dinosaur)] = edgeList.find(!mergedEdgeList.contains(_))

      notMergedEdgeOpt.value shouldEqual ((dinosaurE, dinosaurA))
    }
  }
}
