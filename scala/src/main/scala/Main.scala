import scala.io.{BufferedSource, Source, StdIn}

object Main {

  def main(args: Array[String]): Unit = {
    val bufferedSource: BufferedSource = Source.fromFile("src/main/resources/node_inputs.txt")
    val inputs: Seq[String] = bufferedSource.getLines().toSeq
    bufferedSource.close()
//    val inputs: Seq[String] = Iterator.continually(StdIn.readLine()).takeWhile(_.nonEmpty).toSeq

    val inputsInt: Seq[Int] = inputs.map(_.toInt)

    inputsInt.foreach(println)
  }
}
