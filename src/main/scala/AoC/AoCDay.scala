package AoC

import java.io.{BufferedWriter, File, FileWriter, FilenameFilter}
import scala.io.Source
import scala.util.{Failure, Success, Using}

abstract class AoCDay(year: Int, day: Int) extends App {
  private val testFilePath: String = s"src/main/resources/AoC/year$year/test$day.txt"
  private val inputFilePath: String = s"src/main/resources/AoC/year$year/input$day.txt"
  private lazy val aocUrl: String = s"https://adventofcode.com/$year/day/$day/input"

  private lazy val logFile: File = new File("D:\\Dev\\Scala\\sbt\\AoC_scala\\src\\main\\resources\\log.txt")
  private lazy val bw = new BufferedWriter(new FileWriter(logFile))
  private var logFileEvaluated: Boolean = false

  def testMode: Boolean

  private def downloadInput(): String = {
    import java.net.{HttpURLConnection, URL}
    val connection = new URL(aocUrl).openConnection.asInstanceOf[HttpURLConnection]
    val sessionId = Using(Source.fromFile("src/main/resources/AoC/session.txt")) { src =>
      src.getLines.mkString.trim
    }.get
    connection.setRequestProperty("Cookie", s"session=$sessionId")
    connection.setConnectTimeout(5000)
    connection.setReadTimeout(5000)
    connection.setRequestMethod("GET")
    val inputStream = connection.getInputStream
    val content = Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content
  }

  private def getInputFilePath: String = {
    if (testMode) testFilePath
    else {
      val dir = new File(inputFilePath).getParentFile
      dir
        .listFiles(new FilenameFilter {
          override def accept(dir: File, name: String): Boolean = name == s"input$day.txt"
        })
        .toList match {
        case Nil =>
          val inputFile = new File(inputFilePath)
          val bw = new BufferedWriter(new FileWriter(inputFile))
          val inputContent: String = downloadInput()
          bw.write(inputContent)
          bw.close()
          inputFilePath

        case _ :: Nil => inputFilePath

        case _ => throw new Exception(s"Multiple file with same name: ${inputFilePath.split(File.separator).last}")
      }
    }

  }

  def writeLine(line: String): Unit = {
    logFileEvaluated = true
    bw.write(line)
    bw.newLine()
  }

  def resolveDay(): Unit

  def display(string: String): Unit =
    if (testMode) println(string)
    else ()

  def main(): Unit = {
    resolveDay()
    if (logFileEvaluated) bw.close()
  }

  def getLines: Vector[String] = {
    Using(Source.fromFile(getInputFilePath)) { src =>
      src.getLines.toVector
    } match {
      case Failure(exception) =>
        println(s"Error while reading lines from file (testMode=$testMode): $exception ${exception.getMessage}")
        Vector.empty[String]

      case Success(value) => value
    }
  }

  def getLine: String = {
    Using(Source.fromFile(getInputFilePath)) { src =>
      src.getLines.take(1).toVector
    } match {
      case Failure(exception) =>
        println(s"Error while reading lines from file (testMode=$testMode): $exception ${exception.getMessage}")
        Vector.empty[String].head // will throw an exception

      case Success(value) => value.head.trim
    }
  }

  main()
}
