package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec

object Day11 extends AoCDay(2021, 11) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    lazy val lines = getLines

    case class Octopus(x: Int, y: Int, energy: Int) {
      def incrementEnergy: Octopus = {
        val nextEnergy = energy + 1
        val result = if (nextEnergy > 9) 0 else nextEnergy
        copy(energy = result)
      }
    }

    val octopuses: Vector[Vector[Octopus]] =
      lines.zipWithIndex.map {
        case (line, y) =>
          line.toVector.zipWithIndex.map {
            case (energyLevel, x) => Octopus(x, y, Integer.parseInt(energyLevel.toString))
          }
      }

    def getNeighbours(x: Int, y: Int, octopuses: Vector[Vector[Octopus]]): List[Octopus] =
      (for {
        yNeighbour <- (y - 1) to (y + 1) if yNeighbour >= 0 && yNeighbour < octopuses.length
        xNeighbour <- (x - 1) to (x + 1) if xNeighbour >= 0 && xNeighbour < octopuses.head.length && (xNeighbour, yNeighbour) != (x, y)
      } yield octopuses(yNeighbour)(xNeighbour)).toList

    @tailrec
    def computeStateAfterFlashes(
        remainingFlashingOctopuses: List[Octopus],
        currentOctopusesState: Vector[Vector[Octopus]]
    ): Vector[Vector[Octopus]] =
      remainingFlashingOctopuses match {
        case Nil => currentOctopusesState

        case Octopus(x, y, _) :: tail =>
          val neighboursThatHaventFlashedYet: List[Octopus] = getNeighbours(x, y, currentOctopusesState).filter(_.energy > 0)

          val nextState: Vector[Vector[Octopus]] = neighboursThatHaventFlashedYet
            .foldLeft(currentOctopusesState) {
              case (acc, Octopus(xNeighbour, yNeighbour, _)) =>
                acc.updated[Vector[Octopus]](
                  yNeighbour,
                  acc(yNeighbour).updated[Octopus](xNeighbour, acc(yNeighbour)(xNeighbour).incrementEnergy)
                )
            }

          val newNeighboursFlashing: List[Octopus] = neighboursThatHaventFlashedYet.filter(_.energy == 9)

          computeStateAfterFlashes(newNeighboursFlashing ::: tail, nextState)
      }

    def part1(): Unit = {

      @tailrec
      def step(currentState: Vector[Vector[Octopus]], stepInc: Int = 1, flashes: Int = 0): Int = {
        if (stepInc == 101) flashes
        else {
          val energizedOctopuses: Vector[Vector[Octopus]] = currentState.map(_.map(_.incrementEnergy))
          val octopusesThatNaturallyFlashes: List[Octopus] = energizedOctopuses.flatMap(_.filter(_.energy == 0)).toList
          val nextOctopusesState = computeStateAfterFlashes(
            octopusesThatNaturallyFlashes,
            energizedOctopuses
          )
          val flashesThisStep = nextOctopusesState.flatMap(_.filter(_.energy == 0))

          step(nextOctopusesState, stepInc + 1, flashes + flashesThisStep.length)
        }

      }
      println("")
      println(s"Part 1 = ${step(octopuses)}")
    }

    def part2(): Unit = {
      @tailrec
      def step(currentState: Vector[Vector[Octopus]], stepInc: Int = 1): Int = {
        val energizedOctopuses: Vector[Vector[Octopus]] = currentState.map(_.map(_.incrementEnergy))
        val octopusesThatNaturallyFlashes: List[Octopus] = energizedOctopuses.flatMap(_.filter(_.energy == 0)).toList
        val nextOctopusesState = computeStateAfterFlashes(
          octopusesThatNaturallyFlashes,
          energizedOctopuses
        )

        val flashesThisStep = nextOctopusesState.flatMap(_.filter(_.energy == 0))

        if (flashesThisStep.length == currentState.flatten.length) stepInc
        else step(nextOctopusesState, stepInc + 1)

      }

      println(s"Step2 = ${step(octopuses)}")
    }

//    part1()
    part2()

  }
}
