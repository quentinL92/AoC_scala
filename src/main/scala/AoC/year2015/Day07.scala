package AoC.year2015

import AoC.AoCDay

import scala.collection.mutable

object Day07 extends AoCDay(2015, 7) {
  lazy val OrRegex = """(\w+) OR (\w+)""".r
  lazy val AndRegex = """(\w+) AND (\w+)""".r
  lazy val RshiftRegex = """(\w+) RSHIFT (\d+)""".r
  lazy val LshiftRegex = """(\w+) LSHIFT (\d+)""".r
  lazy val NotRegex = """NOT (\w+)""".r
  lazy val EqualRegex = """(\w+)""".r

  lazy val operations: Map[String, String] = getLines().map { line =>
    val Array(a, b) = line.split(" -> ").map(_.trim)
    b -> a
  }.toMap

  var cache: mutable.Map[String, Int] = mutable.Map.empty[String, Int]

  def getValueFor(wire: String): Int = {
    cache.get(wire) match {
      case Some(value) => value
      case None =>
        val res = operations.get(wire) match {
          case Some(value) =>
            value match {
              case s: String if s.toList.forall(_.isDigit)                    => s.toInt
              case EqualRegex(otherWire)                                      => getValueFor(otherWire)
              case NotRegex(otherWire)                                        => ~getValueFor(otherWire)
              case RshiftRegex(otherWire, value)                              => getValueFor(otherWire) >> value.toInt
              case LshiftRegex(otherWire, value)                              => getValueFor(otherWire) << value.toInt
              case OrRegex(wireA, wireB)                                      => getValueFor(wireA) | getValueFor(wireB)
              case AndRegex(digitA, wireB) if digitA.toList.forall(_.isDigit) => digitA.toInt & getValueFor(wireB)
              case AndRegex(wireA, wireB)                                     => getValueFor(wireA) & getValueFor(wireB)

            }
          case None =>
            println(s"No value found for $wire")
            throw new NoSuchElementException
        }
        cache.put(wire, res)
        res
    }
  }
  override def resolveDay(): Unit = {
    val res1 = getValueFor("a")
    println(f"Res1: $res1")

    cache = mutable.Map("b" -> res1)
    val res2 = getValueFor("a")
    println(f"Res2: $res2")
  }

}
