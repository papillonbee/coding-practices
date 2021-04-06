package coding_practices.model.stubber

import coding_practices.generator.Generator
import coding_practices.model.Dinosaur

object DinosaurStubber extends Generator[Dinosaur] {
  override def build: Dinosaur = {
    Dinosaur(
      name = Generator.alphanumeric.build,
      age = Generator.integer.build,
    )
  }
}
