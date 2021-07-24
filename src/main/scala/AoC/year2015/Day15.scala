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
      accumulatedPossibilites: List[Vector[Int]] = Nil
  ): List[Vector[Int]] = {
    remaingingIngredients match {
      case empty if empty.isEmpty =>
        val score = currentCombination.foldLeft(0) { case (acc, cur) => acc + (cur._1 * property(cur._2)) }
        if (score > 0) currentCombination.map(_._1) :: accumulatedPossibilites
        else Nil

      case lastElem if lastElem.size == 1 =>
        generatePossibilities(
          currentCombination.appended((100 - currentCombination.map(_._1).sum) -> lastElem.head),
          Vector.empty[Ingredient],
          property,
          accumulatedPossibilites
        )

      case moreThanOneElem =>
        (0 until (100 - currentCombination.map(_._1).sum)).foldLeft(List.empty[Vector[Int]]) {
          case (acc, cur) =>
            generatePossibilities(
              currentCombination.appended(cur -> moreThanOneElem.head),
              moreThanOneElem.tail,
              property
            ) ::: acc
        }
    }
  }

  lazy val possibilities: List[Vector[Int]] = (0 until 100).foldLeft(List.empty[Vector[Int]]) {
    case (acc, cur) =>
      generatePossibilities(Vector(cur -> ingredients.head), ingredients.tail, _.capacity) ::: acc
  }

  lazy val properties: List[Ingredient => Int] = List(
    _.capacity, _.durability, _.flavor, _.texture
  )

  lazy val remainingProperty: List[Ingredient => Int] = properties.tail

  def filterPossibilities(
      possibilities: List[Vector[Int]],
      property: Ingredient => Int
                         ): List[Vector[Int]] = {
    possibilities.filter { possibility =>
      val score = ingredients.zip(possibility).foldLeft(0) { case (acc, cur) => acc + (cur._2 * property(cur._1))}
      score > 0
    }
  }

  lazy val finalPossibilities: List[Vector[Int]] = remainingProperty.foldLeft(possibilities) {
    case (acc, cur) => filterPossibilities(acc, cur)
  }

  lazy val finalPossibilitiesMidScore: List[List[Int]] = finalPossibilities.map { possibility =>
    properties.map { property =>
      possibility.zip(ingredients).foldLeft(0) { case (acc, cur) => acc + (cur._1 * property(cur._2)) }
    }
  }

  lazy val finalPossibilitiesScore: List[Int] = finalPossibilitiesMidScore.map(_.product)

  lazy val bestPossibilityScore: Int = finalPossibilitiesScore.max

  lazy val bestPossibily: Vector[Int] =
    finalPossibilities(finalPossibilitiesScore.indexWhere(_ == bestPossibilityScore))

  override def resolveDay(): Unit = {
    println(s"Best possibility: $bestPossibily")
    println(s"Best possibility score: ${bestPossibilityScore}")
  }
}
