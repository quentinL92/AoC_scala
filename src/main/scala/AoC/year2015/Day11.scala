package AoC.year2015

import AoC.AoCDay

import scala.util.matching.Regex

object Day11 extends AoCDay(2015, 11) {

  override lazy val testMode: Boolean = false

  implicit class OnChar(c: Char) {
    lazy val aValue: Int = 'a'.toInt
    lazy val zValue: Int = 'z'.toInt
    lazy val diffValue: Int = zValue - aValue + 1
    def increment: Char = ((c.toInt + 1 - aValue) % diffValue + aValue).toChar
  }

  implicit class OnString(string: String) {
    def increment: String = string match {
      case str if str.isEmpty     => str
      case str if str.last == 'z' => str.dropRight(1).increment + 'a'
      case _                      => string.dropRight(1) + string.last.increment
    }
  }

  lazy val TwoGroupsOfTwoIdenticalChar: Regex = """(?:([a-z])\1.*){2}""".r
  def twoGroupsOfTwoIdenticalCharsValidation(string: String): Boolean =
    TwoGroupsOfTwoIdenticalChar.findFirstIn(string).isDefined

  lazy val forbiddenChars: List[Char] = List('i', 'o', 'l')
  def notContainsForbiddenChars(string: String): Boolean =
    forbiddenChars.forall(forbiddenChar => !(string contains forbiddenChar))

  def tierceOfIncreasingChars(string: String): Boolean =
    string match {
      case str if str.length < 3 => false
      case _ =>
        string.sliding(3).count { str =>
          val headValue = str.head.toInt
          headValue == str(1).toInt - 1 && headValue == str.last.toInt - 2
        } > 0
    }

  def nextPwd(currentPwd: String): String = {
    var pwd: String = currentPwd
    var found: Boolean = false
    val validations: List[String => Boolean] = List(
      twoGroupsOfTwoIdenticalCharsValidation,
      notContainsForbiddenChars,
      tierceOfIncreasingChars
    )
    while (!found) {
      pwd = pwd.increment
      found = validations.forall(_(pwd))
    }
    pwd
  }
  override def resolveDay(): Unit = {
    val input: String = "hepxcrrq"
    val res1 = nextPwd(input)
    println(s"Res1: $res1")

    val res2 = nextPwd(res1)
    println(s"Res2: $res2")
  }
}
