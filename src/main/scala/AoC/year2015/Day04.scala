package AoC.year2015

import java.security.MessageDigest

import AoC.AoCDay

object Day04 extends AoCDay(2015, 4) {
  val input: String = "bgvyzdsv"
  val res1 = LazyList
    .from(1)
    .map(it => MessageDigest.getInstance("MD5").digest(s"$input$it".getBytes).map(0xFF & _).map {
      "%02x".format(_)
    }.mkString(""))
    .indexWhere(_.startsWith("00000")) + 1
  println(res1)
  print(MessageDigest.getInstance("MD5").digest(s"$input$res1".getBytes).map(0xFF & _).map {
    "%02x".format(_)
  }.mkString(""))

  val res2 = LazyList
    .from(1)
    .map(it => MessageDigest.getInstance("MD5").digest(s"$input$it".getBytes).map(0xFF & _).map {
      "%02x".format(_)
    }.mkString(""))
    .indexWhere(_.startsWith("000000")) + 1
  println(res2)
  print(MessageDigest.getInstance("MD5").digest(s"$input$res2".getBytes).map(0xFF & _).map {
    "%02x".format(_)
  }.mkString(""))
}
