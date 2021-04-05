import java.io.{BufferedWriter, FileWriter}
import scala.io.StdIn

object Result {
  def summation(a: Int,b: Int): Int = {
    //Write your code here

    return a + b; //Should return a value of type "Int" from here.
  }
}

object Main {

  def main(args: Array[String]): Unit = {
    var bufferedWriter = new BufferedWriter(new FileWriter("src/main/scala/OUTPUT_FILE_PATH"))
    bufferedWriter.write("\n")
    bufferedWriter.close()
    bufferedWriter = new BufferedWriter(new FileWriter("src/main/scala/OUTPUT_FILE_PATH", true))

    val x: Seq[String] = (1 to 2).map(_ => StdIn.readLine().trim)
    val a = StdIn.readLine.trim.toInt

    val b = StdIn.readLine.trim.toInt

    println(a, b)

    val outcome = Result.summation(a,b)

    println(outcome)

    bufferedWriter.write(String.valueOf(outcome))
    bufferedWriter.write("\n")
    bufferedWriter.close()
  }
}
