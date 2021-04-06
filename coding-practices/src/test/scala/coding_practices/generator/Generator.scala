package coding_practices.generator

import scala.util.Random

trait Generator[T] {
  def build: T

  def buildList(n: Int): Seq[T] = (1 to n).map(_ => build)
}

object Generator {
  def integer: Generator[Int] = new Generator[Int] {
    override def build: Int = Random.nextInt()
  }

  def boolean: Generator[Boolean] = new Generator[Boolean] {
    override def build: Boolean = Random.nextBoolean()
  }

  def double: Generator[Double] = new Generator[Double] {
    override def build: Double = Random.nextDouble()
  }

  def alphanumeric: Generator[String] = new Generator[String] {
    override def build: String = Random.alphanumeric.take(10).mkString
  }
}
