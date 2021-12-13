package AoC.year2015

import AoC.AoCDay
import AoC.year2015.Day16.Aunt.auntSue

import scala.util.matching.Regex

object Day16 extends AoCDay(2015, 16) {

  override lazy val testMode: Boolean = false

  case class Aunt(
      number: Int,
      children: Option[Int] = None,
      cats: Option[Int] = None,
      samoyeds: Option[Int] = None,
      pomeranians: Option[Int] = None,
      akitas: Option[Int] = None,
      vizslas: Option[Int] = None,
      goldfish: Option[Int] = None,
      trees: Option[Int] = None,
      cars: Option[Int] = None,
      perfumes: Option[Int] = None
  ) {
    def matchAunty(): Boolean = {
      (children.isEmpty || auntSue.children.getOrElse(0) == children.getOrElse(0)) &&
      (cats.isEmpty || auntSue.cats.getOrElse(0) == cats.getOrElse(0)) &&
      (samoyeds.isEmpty || auntSue.samoyeds.getOrElse(0) == samoyeds.getOrElse(0)) &&
      (pomeranians.isEmpty || auntSue.pomeranians.getOrElse(0) > pomeranians.getOrElse(0)) &&
      (akitas.isEmpty || auntSue.akitas.getOrElse(0) == akitas.getOrElse(0)) &&
      (vizslas.isEmpty || auntSue.vizslas.getOrElse(0) == vizslas.getOrElse(0)) &&
      (goldfish.isEmpty || auntSue.goldfish.getOrElse(0) == goldfish.getOrElse(0)) &&
      (trees.isEmpty || auntSue.trees.getOrElse(0) == trees.getOrElse(0)) &&
      (cars.isEmpty || auntSue.cars.getOrElse(0) == cars.getOrElse(0)) &&
      (perfumes.isEmpty || auntSue.perfumes.getOrElse(0) == perfumes.getOrElse(0))
    }
    def matchAuntyPart2(): Boolean = {
      (children.isEmpty || auntSue.children.getOrElse(0) == children.getOrElse(0)) &&
      (cats.isEmpty || auntSue.cats.getOrElse(0) < cats.getOrElse(0)) &&
      (samoyeds.isEmpty || auntSue.samoyeds.getOrElse(0) == samoyeds.getOrElse(0)) &&
      (pomeranians.isEmpty || auntSue.pomeranians.getOrElse(0) > pomeranians.getOrElse(0)) &&
      (akitas.isEmpty || auntSue.akitas.getOrElse(0) == akitas.getOrElse(0)) &&
      (vizslas.isEmpty || auntSue.vizslas.getOrElse(0) == vizslas.getOrElse(0)) &&
      (goldfish.isEmpty || auntSue.goldfish.getOrElse(0) > goldfish.getOrElse(0)) &&
      (trees.isEmpty || auntSue.trees.getOrElse(0) < trees.getOrElse(0)) &&
      (cars.isEmpty || auntSue.cars.getOrElse(0) == cars.getOrElse(0)) &&
      (perfumes.isEmpty || auntSue.perfumes.getOrElse(0) == perfumes.getOrElse(0))
    }
  }

  object Aunt {
    lazy val auntSue: Aunt = Aunt(
      0,
      children = Some(3),
      cats = Some(7),
      samoyeds = Some(2),
      pomeranians = Some(3),
      akitas = Some(0),
      vizslas = Some(0),
      goldfish = Some(5),
      trees = Some(3),
      cars = Some(2),
      perfumes = Some(1)
    )

    private def fieldRegex(fieldName: String): Regex = s"(?:.*)$fieldName: (\\d+)(?:.*)".r

    lazy val AuntyRegex: Regex = """Sue (\d+): (.+)""".r
    lazy val ChildrenRegex: Regex = fieldRegex("children")
    lazy val CatsRegex: Regex = fieldRegex("cats")
    lazy val SamoyedsRegex: Regex = fieldRegex("samoyeds")
    lazy val PomeraniansRegex: Regex = fieldRegex("pomeranians")
    lazy val AkitasRegex: Regex = fieldRegex("akitas")
    lazy val VizslasRegex: Regex = fieldRegex("vizslas")
    lazy val GoldfishRegex: Regex = fieldRegex("goldfish")
    lazy val TreesRegex: Regex = fieldRegex("trees")
    lazy val CarsRegex: Regex = fieldRegex("cars")
    lazy val PerfumesRegex: Regex = fieldRegex("perfumes")

    private def extractQuantity(string: String, Regex: Regex): Option[Int] = string match {
      case Regex(quantity) => Some(quantity.toInt)
      case _               => None
    }

    def fromString(string: String): Option[Aunt] = {
      string match {
        case AuntyRegex(number, fields) =>
          Some(
            Aunt(
              number.toInt,
              extractQuantity(fields, ChildrenRegex),
              extractQuantity(fields, CatsRegex),
              extractQuantity(fields, SamoyedsRegex),
              extractQuantity(fields, PomeraniansRegex),
              extractQuantity(fields, AkitasRegex),
              extractQuantity(fields, VizslasRegex),
              extractQuantity(fields, GoldfishRegex),
              extractQuantity(fields, TreesRegex),
              extractQuantity(fields, CarsRegex),
              extractQuantity(fields, PerfumesRegex)
            ))
        case _ => None
      }
    }

  }

  lazy val aunties = getLines.flatMap(Aunt.fromString)

  lazy val possibleAuntSue: Vector[Aunt] = aunties.filter(_.matchAunty())
  lazy val realAuntSue: Aunt = aunties.filter(_.matchAuntyPart2()).head

  override def resolveDay(): Unit = {
    println(s"Possible aunty res 1 =  ${possibleAuntSue.head}")
    println(s"Possible aunty res 2 =  $realAuntSue")
  }
}
