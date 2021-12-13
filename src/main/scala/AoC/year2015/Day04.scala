package AoC.year2015

import AoC.AoCDay

import java.security.MessageDigest

object Day04 extends AoCDay(2015, 4) {

  override lazy val testMode: Boolean = false

  lazy val input: String = "bgvyzdsv"
  lazy val res1 = LazyList
    .from(1)
    .map(
      it =>
        MessageDigest
          .getInstance("MD5")
          .digest(s"$input$it".getBytes)
          .map(0xFF & _)
          .map {
            "%02x".format(_)
          }
          .mkString(""))
    .indexWhere(_.startsWith("00000")) + 1

  lazy val res2 = LazyList
    .from(1)
    .map(
      it =>
        MessageDigest
          .getInstance("MD5")
          .digest(s"$input$it".getBytes)
          .map(0xFF & _)
          .map {
            "%02x".format(_)
          }
          .mkString(""))
    .indexWhere(_.startsWith("000000")) + 1

  override def resolveDay(): Unit = {
    println(res1)
    print(
      MessageDigest
        .getInstance("MD5")
        .digest(s"$input$res1".getBytes)
        .map(0xFF & _)
        .map {
          "%02x".format(_)
        }
        .mkString(""))

    println("-------------------")

    println(res2)
    print(
      MessageDigest
        .getInstance("MD5")
        .digest(s"$input$res2".getBytes)
        .map(0xFF & _)
        .map {
          "%02x".format(_)
        }
        .mkString(""))
  }
}
