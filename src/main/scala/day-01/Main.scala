import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.net.URL
import java.net.HttpURLConnection
import Math.abs

object Main extends App {
  val rawData = Source.fromResource("day-01.txt").getLines().toList
  val list1: ListBuffer[Int] = ListBuffer.empty
  val list2: ListBuffer[Int] = ListBuffer.empty

  rawData.foreach { line =>
    line.split("\\s+") match {
      case Array(a, b) => 
        list1 += a.toInt
        list2 += b.toInt
      case _ => throw new IllegalArgumentException("Invalid input")
    }
  }  

  val sortedList1 = list1.sorted
  val sortedList2 = list2.sorted

  val sumDistances = sortedList1.zip(sortedList2).map { case (a, b) => Math.abs(a - b) }.sum
  val sumDistancesV2 = list1.zip(list2).map { case (a, b) => Math.abs(a - b) }.sum

  println(sumDistances)
}