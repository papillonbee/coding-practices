package coding_practices.model.stubber

import coding_practices.generator.Generator
import coding_practices.model.Dinosaur

class DinosaurStubber() extends Generator[Dinosaur] {
  private var nameValue: Option[String] = None
  private var ageValue: Option[Int] = None

  def name(name: String): DinosaurStubber = {
    nameValue = Some(name)
    this
  }

  def age(age: Int): DinosaurStubber = {
    ageValue = Some(age)
    this
  }

  override def build: Dinosaur = {
    val default: Dinosaur = DinosaurStubber.build
    Dinosaur(
      name = nameValue.getOrElse(default.name),
      age = ageValue.getOrElse(default.age),
    )
  }
}

object DinosaurStubber extends Generator[Dinosaur] {
  def builder: DinosaurStubber = new DinosaurStubber()

  override def build: Dinosaur = {
    Dinosaur(
      name = Generator.alphanumeric.build,
      age = Generator.integer.build,
    )
  }
}
