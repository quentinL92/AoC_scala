package AoC.year2015

import AoC.AoCDay
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.mutable

object Day12Part2 extends AoCDay(2015, 12) {
  val line: String = getLine(test = false)
  println(line)

  val logFile: File = new File("D:\\Dev\\Scala\\sbt\\AoC_scala\\src\\main\\resources\\log.txt")
  val bw = new BufferedWriter(new FileWriter(logFile))

  val openingBrackets: List[Char] = List('{', '[')
  val closingBrackets: List[Char] = List('}', ']')
  val openingArrayBracket: Char = '['
  val openingObjectBracket: Char = '{'

  private def getMatchingBracket(bracket: Char): Char =
    bracket match {
      case '{' => '}'
      case '[' => ']'
      case '}' => '{'
      case ']' => '['
    }

  private def writeLine(line: String): Unit = {
    bw.write(line)
    bw.newLine()
  }

  def extractInts(json: String): Validated[String, List[Int]] = {
    writeLine(s"extractInts: $json")
    if (json.isEmpty) Valid(Nil)
    else {
      json.head match {
        case openingBracket
            if openingBrackets.contains(openingBracket) &&
              json.last == getMatchingBracket(openingBracket) =>
          if (openingBracket == openingArrayBracket) {
            extractIntsFromArray(json.tail.dropRight(1))
          } else {
            extractIntsFromObject(json.tail.dropRight(1))
          }

        case openingBracket if openingBrackets.contains(openingBracket) => Invalid("Invalid Json")
        case '"'                                                        => Valid(Nil)
        case _ =>
          println(s"json not starting with double quote or brackets: $json")
          Valid(List(json.takeWhile(c => c.isDigit || c == '-').toInt))

      }
    }
  }

  private def split(jsonArray: String): List[String] = {
    writeLine("--------split---------")
    val stack = mutable.Stack.empty[Char]
    var currentString = ""
    val res = jsonArray.foldLeft(List.empty[String]) {
      case (acc, cur) if cur == ',' && stack.isEmpty =>
        val newAcc = currentString :: acc
        currentString = ""
        newAcc

      case (acc, cur) if openingBrackets.contains(cur) =>
        stack.push(cur)
        currentString += cur
        acc

      case (acc, cur) if closingBrackets.contains(cur) =>
        stack.pop()
        currentString += cur
        acc

      case (acc, cur) =>
        currentString += cur
        acc

    }
    if (currentString.isEmpty) res
    else currentString :: res
  }

  private def splitAndGetValues(jsonObject: String): List[String] = {
    writeLine(s"splitAndGetValues - input - $jsonObject")
    val res1 = split(jsonObject)
    writeLine(s"split output - $res1")
    val res = res1.map(_.dropWhile(_ != ':').stripPrefix(":"))
    writeLine(s"$res contains 'red' => ${res.contains(""""red"""")}")
    if (res.contains(""""red"""")) {
      writeLine(s"splitAndGetValues contains $res => RED = IGNORED")
      Nil
    } else {
      writeLine(s"splitAndGetValues - output - $res")
      res
    }
  }

  private def extractIntsFromArray(json: String): Validated[String, List[Int]] = {
    writeLine(s"extractIntsFromArray: $json")
    if (json.isEmpty) Valid(Nil)
    else {
      split(json)
        .map(extractInts)
        .sequence
        .map(_.flatten)
    }
  }

  private def extractIntsFromObject(json: String): Validated[String, List[Int]] = {
    writeLine(s"extractIntsFromObject: $json")
    if (json.isEmpty) Valid(Nil)
    else {
      splitAndGetValues(json)
        .map(extractInts)
        .sequence
        .map(_.flatten)
    }
  }

  extractInts(line.replace(" ", "")) match {
    case Valid(ints) =>
      println(s"numbers found: $ints")
      println(s"The sum of all numbers is: ${ints.sum}")
    case Invalid(e) => println(s"ERROR while parsing json: $e")
  }

  bw.close()
}
