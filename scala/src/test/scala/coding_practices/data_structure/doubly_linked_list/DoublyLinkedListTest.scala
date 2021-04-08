package coding_practices.data_structure.doubly_linked_list

import coding_practices.model.Dinosaur
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{OneInstancePerTest, OptionValues}

class DoublyLinkedListTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues {

  describe("DoublyLinkedList") {
    val doublyLinkedList: DoublyLinkedList[Dinosaur] = new DoublyLinkedListImpl[Dinosaur]()

    val first: Dinosaur = Dinosaur(name = "First", age = 22)
    val second: Dinosaur = Dinosaur(name = "Second", age = 23)
    val third: Dinosaur = Dinosaur(name = "Third", age = 8)
    val fourth: Dinosaur = Dinosaur(name = "Fourth", age = 15)
    val fifth: Dinosaur = Dinosaur(name = "Fifth", age = 5)

    it("should add element to the front correctly") {
      doublyLinkedList.pushFront(fifth)
      doublyLinkedList.pushFront(third)
      val frontOpt: Option[Dinosaur] = doublyLinkedList.getFront

      frontOpt.value shouldEqual third
    }

    it("should add element to the back correctly") {
      doublyLinkedList.pushBack(fifth)
      doublyLinkedList.pushBack(third)
      val backOpt: Option[Dinosaur] = doublyLinkedList.getBack

      backOpt.value shouldEqual third
    }

    it("should remove first element correctly") {
      doublyLinkedList.pushBack(second)
      doublyLinkedList.pushBack(third)
      doublyLinkedList.popFront()
      val frontOpt: Option[Dinosaur] = doublyLinkedList.getFront

      frontOpt.value shouldEqual third
    }

    it("should remove last element correctly") {
      doublyLinkedList.pushFront(second)
      doublyLinkedList.pushFront(third)
      doublyLinkedList.popBack()
      val backOpt: Option[Dinosaur] = doublyLinkedList.getBack

      backOpt.value shouldEqual third
    }

    it("should point to the same element if it has size 1") {
      doublyLinkedList.pushBack(second)
      val frontOpt: Option[Dinosaur] = doublyLinkedList.getFront
      val backOpt: Option[Dinosaur] = doublyLinkedList.getBack

      frontOpt.value shouldEqual backOpt.value
    }

    it("should be idempotent when no elements are left to pop") {
      doublyLinkedList.pushFront(first)
      doublyLinkedList.pushBack(second)

      doublyLinkedList.popBack()
      doublyLinkedList.popBack()

      doublyLinkedList.getFront shouldBe empty
      doublyLinkedList.getBack shouldBe empty

      doublyLinkedList.popBack()
      doublyLinkedList.popFront()
      doublyLinkedList.popBack()

      doublyLinkedList.getFront shouldBe empty
      doublyLinkedList.getBack shouldBe empty
    }
  }
}
