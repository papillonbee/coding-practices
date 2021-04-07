package coding_practices.data_structure.hash_map

import coding_practices.model.Dinosaur
import org.scalatest.{OneInstancePerTest, OptionValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class HashMapTest extends AnyFunSpec
  with OneInstancePerTest
  with Matchers
  with OptionValues {

  describe("HashMap") {
    val hashMap: HashMap[String, Dinosaur] = new HashMapImpl[String, Dinosaur]()

    it("should put key and value correctly") {
      val key: String = "myFirstKey"
      val value: Dinosaur = Dinosaur("John", 22)

      hashMap.put(key, value)

      val valueOpt: Option[Dinosaur] = hashMap.get(key)
      valueOpt.value shouldEqual value
    }

    it("should get nothing if key does not exist") {
      val key: String = "myFirstKey"

      val valueOpt: Option[Dinosaur] = hashMap.get(key)
      valueOpt shouldBe empty
    }
  }
}
