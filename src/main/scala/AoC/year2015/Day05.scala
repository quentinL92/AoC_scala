package AoC.year2015

import AoC.AoCDay

import scala.annotation.tailrec

object Day05 extends AoCDay(2015, 5) {

  override lazy val testMode: Boolean = false

  def isNice(string: String, checks: List[String => Boolean]): Boolean = {
    checks.forall(_ (string))
  }

  lazy val vowels: List[Char] = List('a', 'e', 'i', 'o', 'u')
  lazy val notIn: List[String] = List("ab", "cd", "pq", "xy")

  @tailrec
  def checkAdjacent(list: List[Char]): Boolean = list match {
    case Nil => false
    case _ :: Nil => false
    case first :: second :: _ if first == second => true
    case _ :: tail => checkAdjacent(tail)
  }

  def vowelsCheck(str: String): Boolean = vowels.map(vowel => str.count(_ == vowel)).sum >= 3

  def adjacentCheck(str: String): Boolean = checkAdjacent(str.toList)

  def notInCheck(str: String): Boolean = notIn.forall(forbiddenString => !(str contains forbiddenString))

  @tailrec
  def checkAdjacentWithCharBetween(list: List[Char]): Boolean = list match {
    case list if list.length < 3 => false
    case first :: _ :: third :: _ if first == third => true
    case _ :: tail => checkAdjacentWithCharBetween(tail)
  }

  @tailrec
  def checkPairRepeated(list: List[Char]): Boolean = list match {
    case list if list.length < 4 => false
    case first :: second :: tail if tail.mkString("").contains(s"$first$second") => true
    case _ :: tail => checkPairRepeated(tail)
  }

  def adjacentWithCharBetweenCheck(str: String): Boolean = checkAdjacentWithCharBetween(str.toList)

  def pairRepeatedCheck(str: String): Boolean = checkPairRepeated(str.toList)

  override def resolveDay(): Unit = {
    val res1 = getLines.count(isNice(_, List(vowelsCheck, adjacentCheck, notInCheck)))
    println(s"Res1: $res1")

    val res2 = getLines.count(isNice(_, List(adjacentWithCharBetweenCheck, pairRepeatedCheck)))
    println(s"Res2: $res2")
  }
}
