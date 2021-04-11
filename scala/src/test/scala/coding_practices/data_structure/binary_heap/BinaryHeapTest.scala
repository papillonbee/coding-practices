package coding_practices.data_structure.binary_heap

import coding_practices.model.Dinosaur
import coding_practices.model.stubber.DinosaurStubber
import org.scalatest.OptionValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class BinaryHeapTest extends AnyFunSpec
  with Matchers
  with OptionValues {

  describe("BinaryHeap") {
    implicit val customDinosaurOrdering: Ordering[Dinosaur] = Ordering.by[Dinosaur, Int](_.age)
      .orElse(Ordering.by[Dinosaur, String](_.name))

    it("should always pop the maximum element") {
      val binaryHeap: BinaryHeap[Dinosaur] = new BinaryHeapImpl[Dinosaur]()

      val dinosaurs: Seq[Dinosaur] = DinosaurStubber.buildList(10)
      val pushedDinosaurs: Seq[Dinosaur] = dinosaurs.map(binaryHeap.push)

      pushedDinosaurs.sorted.reverse foreach { dinosaur: Dinosaur =>
        val peekedDinosaur: Option[Dinosaur] = binaryHeap.peek
        peekedDinosaur.value shouldEqual dinosaur

        val poppedDinosaur: Option[Dinosaur] = binaryHeap.pop()
        poppedDinosaur.value shouldEqual dinosaur
      }
    }

    it("should build heap from its constructor during initialization") {
      val dinosaurs: Seq[Dinosaur] = DinosaurStubber.buildList(10)

      val binaryHeap: BinaryHeap[Dinosaur] = new BinaryHeapImpl[Dinosaur](dinosaurs: _*)

      dinosaurs.sorted.reverse foreach { dinosaur: Dinosaur =>
        val peekedDinosaur: Option[Dinosaur] = binaryHeap.peek
        peekedDinosaur.value shouldEqual dinosaur

        val poppedDinosaur: Option[Dinosaur] = binaryHeap.pop()
        poppedDinosaur.value shouldEqual dinosaur
      }
    }

    it("should be idempotent when no elements are left to pop") {
      val dinosaurs: Seq[Dinosaur] = DinosaurStubber.buildList(10)

      val binaryHeap: BinaryHeap[Dinosaur] = new BinaryHeapImpl[Dinosaur](dinosaurs: _*)

      dinosaurs.foreach(_ => binaryHeap.pop())
      val poppedDinosaurs: Seq[Dinosaur] = dinosaurs.flatMap(_ => binaryHeap.pop())

      poppedDinosaurs shouldBe empty
    }
  }
}
