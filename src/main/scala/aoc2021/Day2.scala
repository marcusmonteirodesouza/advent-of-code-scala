package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day2 {
  abstract class Command

  case class Forward(x: Int) extends Command

  case class Down(y: Int) extends Command

  case class Up(y: Int) extends Command

  case class Position(x: Int, y: Int)

  private def readPlannedCourse: List[Command] =
    Source
      .fromResource("aoc2021/day-2.txt")
      .getLines()
      .map(line => {
        line.split(" ") match {
          case Array("forward", x) => Forward(x.toInt)
          case Array("down", x)    => Down(x.toInt)
          case Array("up", x)      => Up(x.toInt)
        }
      })
      .toList

  def partOne: Int = {
    def runCourse(plannedCourse: List[Command]): Position = {
      def executeCommand(command: Command, position: Position): Position = {
        command match {
          case Forward(x) => position.copy(x = position.x + x)
          case Down(x)    => position.copy(y = position.y + x)
          case Up(x)      => position.copy(y = position.y - x)
        }
      }

      @tailrec
      def runCourseGo(runningCourse: List[Command],
                      currentPosition: Position): Position = {
        runningCourse match {
          case Nil => currentPosition;
          case command :: rest =>
            runCourseGo(rest, executeCommand(command, currentPosition))
        }
      }

      runCourseGo(plannedCourse, Position(0, 0))
    }

    val plannedCourse = readPlannedCourse
    val finalPosition = runCourse(plannedCourse)
    finalPosition.x * finalPosition.y
  }

  def partTwo: Int = {
    type Aim = Int
    case class State(position: Position, aim: Aim)

    def runCourse(plannedCourse: List[Command]): State = {
      def executeCommand(command: Command, state: State): State = {
        command match {
          case Forward(x) =>
            state.copy(
              position = Position(state.position.x + x,
                                  state.position.y + state.aim * x))
          case Down(x) => state.copy(aim = state.aim + x)
          case Up(x)   => state.copy(aim = state.aim - x)
        }
      }

      @tailrec
      def runCourseGo(runningCourse: List[Command],
                      currentState: State): State = {
        runningCourse match {
          case Nil => currentState;
          case command :: rest =>
            runCourseGo(rest, executeCommand(command, currentState))
        }
      }

      runCourseGo(plannedCourse, State(Position(0, 0), 0))
    }

    val plannedCourse = readPlannedCourse
    val finalState = runCourse(plannedCourse)
    finalState.position.x * finalState.position.y
  }
}
