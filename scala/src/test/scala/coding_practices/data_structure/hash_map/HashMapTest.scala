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

    it("should put existing key and overwrite existing value with new value correctly") {
      val key: String = "myFirstKey"
      val value1: Dinosaur = Dinosaur("John", 22)
      val value2: Dinosaur = Dinosaur("Paul", 5)

      hashMap.put(key, value1)
      hashMap.put(key, value2)

      val valueOpt: Option[Dinosaur] = hashMap.get(key)
      valueOpt.value shouldEqual value2
    }

    it("should get nothing if key does not exist") {
      val key: String = "myFirstKey"

      val valueOpt: Option[Dinosaur] = hashMap.get(key)
      valueOpt shouldBe empty
    }
  }
}
