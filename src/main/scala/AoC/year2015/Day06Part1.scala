package AoC.year2015

import AoC.AoCDay

import scala.util.matching.Regex

case class Position(x: Int, y: Int)

sealed trait Instruction {
  def start: Position

  def last: Position
}

case class TurnOn(start: Position, last: Position) extends Instruction

case class TurnOff(start: Position, last: Position) extends Instruction

case class Toggle(start: Position, last: Position) extends Instruction

object Day06Part1 extends AoCDay(2015, 6) {
  val InstructionLine: Regex = """(turn on|toggle|turn off) (\d+),(\d+) through (\d+),(\d+)""".r
  lazy val instructions: List[Instruction] = getLines().toList.map {
    case InstructionLine(instruction, fromx, fromy, tox, toy) if instruction == "turn on" =>
      TurnOn(Position(fromx.toInt, fromy.toInt), Position(tox.toInt, toy.toInt))
    case InstructionLine(instruction, fromx, fromy, tox, toy) if instruction == "turn off" =>
      TurnOff(Position(fromx.toInt, fromy.toInt), Position(tox.toInt, toy.toInt))
    case InstructionLine(instruction, fromx, fromy, tox, toy) if instruction == "toggle" =>
      Toggle(Position(fromx.toInt, fromy.toInt), Position(tox.toInt, toy.toInt))
  }
  lazy val lightsLit: Set[Position] = instructions.foldLeft(Set.empty[Position]) {
    case (lightsLit, TurnOn(start, last)) =>
      lightsLit ++ (for (x <- start.x to last.x; y <- start.y to last.y) yield Position(x, y)).toSet
    case (lightsLit, TurnOff(start, last)) =>
      lightsLit -- (for (x <- start.x to last.x; y <- start.y to last.y) yield Position(x, y)).toSet
    case (lightsLit, Toggle(start, last)) =>
      (for {
        x <- start.x to last.x
        y <- start.y to last.y
      } yield Position(x, y)).foldLeft(lightsLit) {
        case (set, position: Position) if set(position) => set - position
        case (set, position: Position) => set + position
      }
  }
//  println(s"Res1: {lightsLit.size}")


}
