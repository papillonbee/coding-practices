package coding_practices.data_structure.linked_list

import coding_practices.model.Dinosaur
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{OneInstancePerTest, OptionValues, PrivateMethodTester}

class LinkedListTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues
  with PrivateMethodTester {

  val findNode: PrivateMethod[Option[Node[Dinosaur]]] = PrivateMethod('findNode)

  describe("LinkedList") {
    val linkedList: LinkedList[Dinosaur] = new LinkedListImpl[Dinosaur]()

    val john: Dinosaur = Dinosaur(name = "John", age = 22)
    val paul: Dinosaur = Dinosaur(name = "Paul", age = 23)
    val jack: Dinosaur = Dinosaur(name = "Jack", age = 8)

    val dinosaurs: Seq[Dinosaur] = Seq(
      john,
      paul,
      jack,
    )

    it("should add element correctly") {
      dinosaurs.foreach(linkedList.add)

      val foundDinosaurs: Seq[Dinosaur] = dinosaurs.flatMap(linkedList.find)

      foundDinosaurs should contain theSameElementsAs dinosaurs
    }

    it("should remove element correctly") {
      dinosaurs.foreach(linkedList.add)
      linkedList.remove(paul)

      val foundDinosaur: Option[Dinosaur] = linkedList.find(paul)

      foundDinosaur shouldBe empty
    }

    it("should store elements correctly") {
      dinosaurs.foreach(linkedList.add)
      linkedList.remove(paul)

      val nodeOpt: Option[Node[Dinosaur]] = linkedList invokePrivate findNode(jack)
      val dinosaurOpt: Option[Dinosaur] = nodeOpt.map(_.value)

      val nextNodeOpt: Option[Node[Dinosaur]] = nodeOpt.flatMap(_.getNextNode)
      val nextDinosaurOpt = nextNodeOpt.map(_.value)

      dinosaurOpt.value shouldEqual jack
      nextDinosaurOpt.value shouldEqual john
    }
  }
}
