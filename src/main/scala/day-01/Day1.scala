import scala.io.Source
import scala.collection.mutable.ListBuffer
import java.net.URL
import java.net.HttpURLConnection
import Math.abs

object Day1 extends App {
  
  val rawData = Source.fromResource("day-01.txt").getLines().toList

  val (listA, listB) = rawData.map { line =>
      val Array(a, b) = line.split("\\s+")
      (a.toInt, b.toInt)
    }.toList.unzip  

  val sumDistances = listA.sorted.zip(listB.sorted).map { case (a, b) => Math.abs(a - b) }.sum
  println(sumDistances)

  val similarityScrore = listA.foldLeft(0) { (acc, a) =>
    acc + (listB.count(_ == a) * a)
  }
  println(similarityScrore)


}