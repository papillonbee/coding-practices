package coding_practices.generator

import java.util.UUID
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

  def uuid: Generator[UUID] = new Generator[UUID] {
    override def build: UUID = UUID.randomUUID()
  }

  def option[T](
    generator: Generator[T],
    nonNullProbability: Double = 0.5,
  ): Generator[Option[T]] = new Generator[Option[T]] {
    override def build: Option[T] = if (Random.nextDouble() < nonNullProbability) Some(generator.build) else None
  }

  def choose[T](values: T*): T = Random.shuffle(values).head

  implicit def toGenerator[T](t: T): Generator[T] = new Generator[T] {
    override def build: T = t
  }
}
