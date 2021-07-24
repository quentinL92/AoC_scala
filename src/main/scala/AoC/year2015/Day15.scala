package AoC.year2015

import AoC.AoCDay

import scala.util.matching.Regex

object Day15 extends AoCDay(2015, 15) {
  lazy val isTest: Boolean = false
  lazy val lines = getLines(test = isTest)

  lazy val IngredientRegex: Regex =
    """(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)""".r

  case class Ingredient(name: String, capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  lazy val ingredients: Vector[Ingredient] = lines.map {
    case IngredientRegex(name, capacity, durability, flavor, texture, calories) =>
      Ingredient(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }

  lazy val maxTeaspoons: Int = 100

  def generatePossibilities(
      currentCombination: Vector[(Int, Ingredient)],
      remaingingIngredients: Vector[Ingredient],
      property: Ingredient => Int,
      keepFunction: Int => Boolean,
      accumulatedPossibilites: List[Vector[Int]] = Nil
  ): List[Vector[Int]] = {
    remaingingIngredients match {
      case empty if empty.isEmpty =>
        val score = currentCombination.foldLeft(0) { case (acc, cur) => acc + (cur._1 * property(cur._2)) }
        if (keepFunction(score)) currentCombination.map(_._1) :: accumulatedPossibilites
        else Nil

      case lastElem if lastElem.size == 1 =>
        generatePossibilities(
          currentCombination.appended((100 - currentCombination.map(_._1).sum) -> lastElem.head),
          Vector.empty[Ingredient],
          property,
          keepFunction,
          accumulatedPossibilites
        )

      case moreThanOneElem =>
        (0 until (100 - currentCombination.map(_._1).sum)).foldLeft(List.empty[Vector[Int]]) {
          case (acc, cur) =>
            generatePossibilities(
              currentCombination.appended(cur -> moreThanOneElem.head),
              moreThanOneElem.tail,
              property,
              keepFunction,
              accumulatedPossibilites
            ) ::: acc
        }
    }
  }

  def possibilities(keepFunction: Int => Boolean, property: Ingredient => Int): List[Vector[Int]] =
    (0 until 100).foldLeft(List.empty[Vector[Int]]) {
      case (acc, cur) =>
        generatePossibilities(Vector(cur -> ingredients.head), ingredients.tail, property, keepFunction) ::: acc
    }

  lazy val properties: List[Ingredient => Int] = List(
    _.capacity,
    _.durability,
    _.flavor,
    _.texture
  )

  lazy val remainingProperty: List[Ingredient => Int] = properties.tail

  def filterPossibilities(
      possibilities: List[Vector[Int]],
      property: Ingredient => Int
  ): List[Vector[Int]] = {
    possibilities.filter { possibility =>
      val score = ingredients.zip(possibility).foldLeft(0) { case (acc, cur) => acc + (cur._2 * property(cur._1)) }
      score > 0
    }
  }

  lazy val finalPossibilitiesPart1: List[Vector[Int]] =
    remainingProperty.foldLeft(possibilities(_ > 0, _.capacity)) {
      case (acc, cur) => filterPossibilities(acc, cur)
    }

  def finalPossibilitiesMidScore(finalPossibilities: List[Vector[Int]]): List[List[Int]] =
    finalPossibilities.map { possibility =>
      properties.map { property =>
        possibility.zip(ingredients).foldLeft(0) { case (acc, cur) => acc + (cur._1 * property(cur._2)) }
      }
    }

  lazy val finalPossibilitiesMidScorePart1: List[List[Int]] = finalPossibilitiesMidScore(finalPossibilitiesPart1)

  lazy val finalPossibilitiesScorePart1: List[Int] =
    finalPossibilitiesMidScorePart1.map(_.product)

  lazy val bestPossibilityScorePart1: Int = finalPossibilitiesScorePart1.max

  lazy val bestPossibily: Vector[Int] =
    finalPossibilitiesPart1(finalPossibilitiesScorePart1.indexWhere(_ == bestPossibilityScorePart1))

  /*** Part 2 ***/
  lazy val finalPossibilitiesPart2: List[Vector[Int]] =
    properties.foldLeft(possibilities(_ == 500, _.calories)) {
      case (acc, cur) => filterPossibilities(acc, cur)
    }

  lazy val finalPossibilitiesScorePart2: List[Int] = finalPossibilitiesMidScore(finalPossibilitiesPart2).map(_.product)
  lazy val bestPossibilityScorePart2: Int = finalPossibilitiesScorePart2.max
  lazy val bestPossibilyPart2: Vector[Int] =
    finalPossibilitiesPart2(finalPossibilitiesScorePart2.indexWhere(_ == bestPossibilityScorePart2))

  override def resolveDay(): Unit = {
//    println(s"Best possibility: $bestPossibily")
//    println(s"Best possibility score: $bestPossibilityScorePart1")
    println(s"Best possibility: $bestPossibilyPart2")
    println(s"Best possibility score: $bestPossibilityScorePart2")
  }
}
