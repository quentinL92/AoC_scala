package AoC

import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import scala.util.{Failure, Success, Using}

abstract class AoCDay(year: Int, day: Int) extends App {
  private val testFileName: String = s"src/main/resources/AoC/year$year/test$day.txt"
  private val inputFileName: String = s"src/main/resources/AoC/year$year/input$day.txt"

  private lazy val logFile: File = new File("D:\\Dev\\Scala\\sbt\\AoC_scala\\src\\main\\resources\\log.txt")
  private lazy val bw = new BufferedWriter(new FileWriter(logFile))
  private var logFileEvaluated: Boolean = false

  def writeLine(line: String): Unit = {
    logFileEvaluated = true
    bw.write(line)
    bw.newLine()
  }

  def resolveDay(): Unit

  def main(): Unit = {
    resolveDay()
    if (logFileEvaluated) bw.close()
  }

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

  main()
}
