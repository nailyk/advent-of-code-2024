import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.net.URL
import java.net.HttpURLConnection
import Math.abs
import scala.util.matching.Regex

object Day3 extends App {

  val rawData = Source.fromResource("day-03.txt").getLines().toList

  def extractMulGroups(text: String): List[(Int, Int)] = {
    val pattern = new Regex("""mul\((\d{1,3}),(\d{1,3})\)""")
    pattern
      .findAllMatchIn(text)
      .map { m =>
        (m.group(1).toInt, m.group(2).toInt)
      }
      .toList
  }

  def extractMulGroupsV2(text: String): List[(Int, Int)] = {
    val pattern = new Regex(
      """(mul\((\d{1,3}),(\d{1,3})\))|(do\(\))|(don't\(\))"""
    )
    var mulEnabled = true
    val mulGroups = ListBuffer[(Int, Int)]()

    pattern.findAllMatchIn(text).foreach { m =>
      if (m.group(2) != null && m.group(3) != null) {
        if (mulEnabled) {
          mulGroups += ((m.group(2).toInt, m.group(3).toInt))
        }
      } else if (m.group(4) != null) {
        mulEnabled = true
      } else if (m.group(5) != null) {
        mulEnabled = false
      }
    }
    mulGroups.toList
  }

  val sum = extractMulGroups(rawData.mkString).map { case (a, b) => a * b }.sum
  println(sum)

  val sum2 = extractMulGroupsV2(rawData.mkString).map { case (a, b) =>
    a * b
  }.sum
  println(sum2)
}
