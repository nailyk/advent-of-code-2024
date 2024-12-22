import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.net.URL
import java.net.HttpURLConnection
import Math.abs
import scala.util.matching.Regex

object Day4 extends App {

  val rawData = Source.fromResource("day-04.txt").getLines().toList
  val word = List('X', 'M', 'A', 'S')
  val chars = rawData.map(_.toList)

  val directions = List(
    (0, 1), // horizontal right
    (0, -1), // horizontal left
    (1, 0), // vertical down
    (-1, 0), // vertical up
    (1, 1), // diagonal down-right
    (1, -1), // diagonal down-left
    (-1, 1), // diagonal up-right
    (-1, -1) // diagonal up-left
  )

  def isWordPresent(i: Int, j: Int, di: Int, dj: Int): Boolean = {
    val word = Day4.word
    word.indices.forall(k =>
      i + k * di >= 0 && i + k * di < chars.length &&
        j + k * dj >= 0 && j + k * dj < chars(i).length &&
        chars(i + k * di)(j + k * dj) == word(k)
    )
  }

  def isXmasTreePattern(i: Int, j: Int): Boolean = {
    chars(i)(j) == 'A' &&
    List("MSMS", "MMSS", "SMSM", "SSMM").contains(
      s"${chars(i - 1)(j - 1)}${chars(i + 1)(j - 1)}${chars(i - 1)(j + 1)}${chars(i + 1)(j + 1)}"
    )
  }

  val count = (for {
    i <- 1 until chars.length - 1
    j <- 1 until chars.length - 1
    (di, dj) <- directions
    if isWordPresent(i, j, di, dj)
  } yield 1).sum

  println(count)

  val count2 = (for {
    i <- 1 until chars.length - 1
    j <- 1 until chars.length - 1
    if isXmasTreePattern(i, j)
    if isXmasTreePattern(i, j)
  } yield 1).sum

  println(count2)
}
