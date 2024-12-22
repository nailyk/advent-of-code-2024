import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.net.URL
import java.net.HttpURLConnection
import Math.abs

object Day2 extends App {
  
  val rawData = Source.fromResource("day-02.txt").getLines().toList
  
  case class Level(value: Int) extends AnyVal
  case class Report(levels: List[Level]) {
    def isSafe: Boolean = {
      val pairs = levels.sliding(2).toList
      var increasing = true
      var decreasing = true
      pairs.forall {
        case List(a, b) =>
          val diff = Math.abs(a.value - b.value)
          if (diff < 1 || diff > 3) return false
          if (a.value >= b.value) increasing = false
          if (a.value <= b.value) decreasing = false
          true
        case _ => true
      } && (increasing || decreasing)
    }
  }

  val reports = rawData.map { line =>
      Report(line.split("\\s+").map(_.toInt).map(Level).toList)
  }.toList
      
    println(reports.count(_.isSafe))
}