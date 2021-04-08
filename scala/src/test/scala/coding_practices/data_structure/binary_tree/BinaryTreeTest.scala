package coding_practices.data_structure.binary_tree

import coding_practices.model.Dinosaur
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{OneInstancePerTest, OptionValues, PrivateMethodTester}

class BinaryTreeTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues
  with PrivateMethodTester {

  val findNode: PrivateMethod[Option[Node[Dinosaur]]] = PrivateMethod('findNode)

  describe("BinaryTree") {
    val dinosaurCustomOrdering: Ordering[Dinosaur] = (x: Dinosaur, y: Dinosaur) => {
      if (x.age < y.age) -1
      else if (x.age == y.age) 0
      else 1
    }
    val binaryTree: BinaryTree[Dinosaur] = new BinaryTreeImpl[Dinosaur]()(dinosaurCustomOrdering)

    val john: Dinosaur = Dinosaur(name = "John", age = 22)
    val paul: Dinosaur = Dinosaur(name = "Paul", age = 23)
    val jack: Dinosaur = Dinosaur(name = "Jack", age = 8)
    val john2: Dinosaur = Dinosaur(name = "John", age = 22)

    val dinosaurs: Seq[Dinosaur] = Seq(
      john,
      paul,
      jack,
      john2,
    )

    it("should add element correctly") {
      dinosaurs.foreach(binaryTree.add)

      val foundDinosaurs: Seq[Dinosaur] = dinosaurs.flatMap(binaryTree.find)

      foundDinosaurs should contain theSameElementsAs dinosaurs
    }

    it("should remove element correctly") {
      dinosaurs.foreach(binaryTree.add)
      binaryTree.remove(paul)

      val foundDinosaur: Option[Dinosaur] = binaryTree.find(paul)

      foundDinosaur shouldBe empty
    }

    it("should store elements correctly") {
      dinosaurs.foreach(binaryTree.add)
      binaryTree.remove(paul)

      val nodeOpt: Option[Node[Dinosaur]] = binaryTree invokePrivate findNode(john)
      val leftNodeOpt: Option[Node[Dinosaur]] = nodeOpt.flatMap(_.getLeftNode)
      val rightNodeOpt: Option[Node[Dinosaur]] = nodeOpt.flatMap(_.getRightNode)

      val dinosaurOpt: Option[Dinosaur] = nodeOpt.map(_.value)
      val leftDinosaurOpt: Option[Dinosaur] = leftNodeOpt.map(_.value)
      val rightDinosaurOpt: Option[Dinosaur] = rightNodeOpt.map(_.value)

      dinosaurOpt.value shouldEqual john
      leftDinosaurOpt.value shouldEqual jack
      rightDinosaurOpt.value shouldEqual john2
    }
  }
}
