package coding_practices.algorithm

import coding_practices.model.Dinosaur
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class SortingTest extends AnyFunSpec
  with Matchers {

  describe("mergeSort") {
    it("should sort correctly") {
      val list: List[Dinosaur] = List(
        Dinosaur("a", 99),
        Dinosaur("b", 2),
        Dinosaur("c", 5),
        Dinosaur("d", 3),
        Dinosaur("e", 101),
        Dinosaur("f", 5),
      )

      val customDinosaurOrdering: Ordering[Dinosaur] = Ordering.by[Dinosaur, Int](_.age)
        .orElseBy[String](_.name)
      val sortedList: List[Dinosaur] = Sorting.mergeSort(list)(customDinosaurOrdering)

      sortedList should contain theSameElementsInOrderAs list.sorted(customDinosaurOrdering)
    }
  }
}
