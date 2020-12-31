package AoC

import scala.io.Source
import scala.util.{Failure, Success, Using}

abstract class AoCDay(year: Int, day: Int) extends App {
  val testFileName: String = s"src/main/resources/AoC/year$year/test$day.txt"
  val inputFileName: String = s"src/main/resources/AoC/year$year/input$day.txt"

  def getLines(test: Boolean = false): Vector[String] = {
    val fileName: String = if (test) testFileName else inputFileName
    Using(Source.fromFile(fileName)) { src =>
      src.getLines().toVector
    } match {
      case Failure(exception) =>
        println(s"Error while reading lines from $fileName: $exception")
        Vector.empty[String]
      case Success(value) => value
    }
  }

  def getLine(test: Boolean = false): String = {
    val fileName: String = if (test) testFileName else inputFileName
    Using(Source.fromFile(fileName)) { src =>
      src.getLines().take(1).toVector
    } match {
      case Failure(exception) =>
        println(s"Error while reading lines from $fileName: $exception ${exception.getMessage}")
        Vector.empty[String].head // will throw an exception

      case Success(value) => value.head.trim
    }
  }
}
