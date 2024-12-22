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
    
  def isSafeFunctional: Boolean = {
    @annotation.tailrec
    def loop(pairs: List[List[Level]], allLevelsIncreasing: Boolean, allLevelsDecreasing: Boolean): Boolean = pairs match {
      case List(a, b) :: tail =>
        val diff = Math.abs(a.value - b.value)
        if (diff < 1 || diff > 3) false
        else loop(tail, allLevelsIncreasing && a.value <= b.value, allLevelsDecreasing && a.value >= b.value)
      case _ => allLevelsIncreasing || allLevelsDecreasing
    }
    loop(levels.sliding(2).toList, allLevelsIncreasing = true, allLevelsDecreasing = true)
  }

  def isSafeFunctionalV2: Boolean = {
    def isSafeWithOneRemoval: Boolean = {
      @annotation.tailrec
      def loop(remaining: List[Level], checked: List[Level]): Boolean = remaining match {
        case Nil => false
        case _ :: tail =>
          val newLevels = checked ++ tail
          if (Report(newLevels).isSafeFunctional) true
          else loop(tail, checked :+ remaining.head)
      }
      loop(levels, Nil)
    }
    isSafeFunctional || isSafeWithOneRemoval
  }
  
}

  val reports = rawData.map { line =>
      Report(line.split("\\s+").map(_.toInt).map(Level).toList)
  }.toList
      
  println(reports.count(_.isSafe))
  println(reports.count(_.isSafeFunctional))
  println(reports.count(_.isSafeFunctionalV2))
}