package AoC.year2021

import AoC.AoCDay

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day04 extends AoCDay(2021, 4) {

  override lazy val testMode: Boolean = false

  override def resolveDay(): Unit = {

    val lines = getLines
    val numbers: List[Int] = lines.head.split(',').map(_.toInt).toList

    case class BingoBoardValue(value: Int, checked: Boolean = false)

    val BingoLineRegex: Regex = """^\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r

    val bingoBoards: Vector[Vector[Array[BingoBoardValue]]] = lines
      .drop(2)
      .sliding(5, 6)
      .map(_.map {
        case BingoLineRegex(first, second, third, fourth, fifth) =>
          Array(first, second, third, fourth, fifth)
            .map(value => BingoBoardValue(value.toInt))
      })
      .toVector

    def getWinningBoard(bingoBoards: Vector[Vector[Array[BingoBoardValue]]]): Option[Vector[Array[BingoBoardValue]]] = {
      bingoBoards.find { bingoBoard =>
        bingoBoard.indices.exists { index =>
          bingoBoard(index).forall(_.checked) || bingoBoard.forall(a => a(index).checked)
        }
      }
    }

    def filterWinningBoards(bingoBoards: List[Vector[Array[BingoBoardValue]]]): List[Vector[Array[BingoBoardValue]]] =
      bingoBoards
        .filterNot { bingoBoard =>
          bingoBoard.indices.exists { index =>
            bingoBoard(index).forall(_.checked) || bingoBoard.forall(a => a(index).checked)
          }
        }

    def playOneRound(
        updatedBingoBoards: Vector[Vector[Array[BingoBoardValue]]],
        head: Int
    ): Vector[Vector[Array[BingoBoardValue]]] = {
      updatedBingoBoards.map { bingoBoard =>
        bingoBoard.map(_.map {
          case number if number.value == head && !number.checked => number.copy(checked = true)
          case number                                            => number
        })
      }
    }

    def playOneRoundList(
        updatedBingoBoards: List[Vector[Array[BingoBoardValue]]],
        head: Int
    ): List[Vector[Array[BingoBoardValue]]] = {
      updatedBingoBoards.map { bingoBoard =>
        bingoBoard.map(_.map {
          case number if number.value == head && !number.checked => number.copy(checked = true)
          case number                                            => number
        })
      }
    }

    @tailrec
    def resolvePart1(
        updatedBingoBoards: Vector[Vector[Array[BingoBoardValue]]],
        remainingNumbers: List[Int]
    ): (Vector[Array[BingoBoardValue]], Int) = remainingNumbers match {
      case Nil => throw new IndexOutOfBoundsException(s"No winners")
      case head :: tail =>
        val nextUpdatedBingoBoards = playOneRound(updatedBingoBoards, head)

        getWinningBoard(nextUpdatedBingoBoards) match {
          case Some(value) => value -> head
          case None        => resolvePart1(nextUpdatedBingoBoards, tail)
        }
    }

    @tailrec
    def resolvePart2(
        remainingBingoBoards: List[Vector[Array[BingoBoardValue]]],
        remainingNumbers: List[Int]
    ): (Vector[Array[BingoBoardValue]], Int) =
      remainingNumbers match {
        case Nil => throw new IndexOutOfBoundsException(s"No winners")

        case headNumber :: tailNumber =>
          playOneRoundList(
            filterWinningBoards(remainingBingoBoards),
            headNumber
          ) match {
            case Nil => throw new NoSuchElementException(s"No last winning board")

            case head :: Nil =>
              getWinningBoard(Vector(head)) match {
                case Some(value) => value -> headNumber
                case None        => resolvePart2(List(head), tailNumber)
              }

            case multipleValues => resolvePart2(multipleValues, tailNumber)
          }
      }

    def computeScore(board: Vector[Array[BingoBoardValue]], number: Int): Int = board.map(_.collect {
      case BingoBoardValue(value, checked) if !checked => value
    }.sum)
      .sum * number

    lazy val (winningBoard, lastNumber) = resolvePart1(bingoBoards, numbers)
    lazy val resPart1 = computeScore(winningBoard, lastNumber)

    lazy val (loosingBoardPart2, lastNumberPart2) = resolvePart2(bingoBoards.toList, numbers)
    lazy val resPart2 = computeScore(loosingBoardPart2, lastNumberPart2)

    println(s"Part1 = $resPart1")
    println(s"Part2 = $resPart2")

  }
}
