package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day4 {
  private type BingoBoardGrid = Seq[Seq[Int]]

  private class BingoGame(numbers: Seq[Int], grids: Seq[BingoBoardGrid]) {
    def findWinners: Seq[BingoBoard] = {
      @tailrec
      def findWinnersGo(numbersToGo: Seq[Int],
                        runners: Seq[BingoBoard],
                        winners: Seq[BingoBoard]): Seq[BingoBoard] = {
        numbersToGo match {
          case Nil => winners
          case number +: rest =>
            runners.foreach { board =>
              board.mark(number)
            }
            runners.find(_.hasWon) match {
              case Some(board) =>
                findWinnersGo(rest, runners.filter(!_.hasWon), winners :+ board)
              case None => findWinnersGo(rest, runners, winners)
            }
        }
      }

      val boards: Seq[BingoBoard] =
        grids.map(grid => new BingoBoard(grid))

      findWinnersGo(numbers, boards, List())
    }
  }

  private class BingoBoard(grid: BingoBoardGrid) {
    require(grid.size == 5 && grid.forall(_.size == 5), "Grid must be 5x5")

    private var marked = Array[Int]()

    private var unmarked = grid.flatten.toSet

    private def rows: Seq[Seq[Int]] = grid

    private def columns: Seq[Seq[Int]] = {
      @tailrec
      def columnsGo(grid: BingoBoardGrid,
                    column: Int,
                    columns: Seq[Seq[Int]]): Seq[Seq[Int]] = {
        if (column >= grid.size) {
          return columns
        }
        columnsGo(grid, column + 1, columns :+ grid.map(_(column)))
      }
      columnsGo(grid, 0, List())
    }

    private def diagonals: Seq[Seq[Int]] = {
      @tailrec
      def diagonalsGo(grid: BingoBoardGrid,
                      index: Int,
                      diagonalNumbersByRow: Seq[(Int, Int)]): Seq[Seq[Int]] = {
        if (index >= grid.size) {
          return List(diagonalNumbersByRow.map(_._1),
                      diagonalNumbersByRow.map(_._2))
        }
        diagonalsGo(grid,
                    index + 1,
                    diagonalNumbersByRow :+ (grid(index)(index), grid(index)(
                      grid.size - 1 - index)))
      }
      diagonalsGo(grid, 0, List())
    }

    def mark(number: Int): Unit = {
      if (unmarked(number)) {
        marked = marked :+ number
        unmarked -= number
      }
    }

    def hasWon: Boolean = {
      rows.exists(row => row.forall(number => marked.contains(number))) || columns
        .exists(column => column.forall(number => marked.contains(number))) || diagonals
        .exists(diagonal => diagonal.forall(number => marked.contains(number)))
    }

    def score: Int = {
      marked.last * unmarked.sum
    }
  }

  private def readBingoGame = {
    val lines = Source.fromResource("aoc2021/day-4.txt").getLines().toList
    val numbers = lines.head.split(',').toSeq.map(Integer.parseInt)
    val grids = lines
      .drop(2)
      .sliding(5, 6)
      .map(
        grid =>
          grid
            .take(5)
            .map(
              row =>
                row
                  .split(' ')
                  .toList
                  .filter(n => !n.isBlank)
                  .map(Integer.parseInt)))
      .toSeq
    new BingoGame(numbers, grids)
  }

  def partOne: Option[Int] = {
    val bingo = readBingoGame
    bingo.findWinners.headOption match {
      case Some(winner) => Some(winner.score)
      case None         => None
    }
  }

  def partTwo: Option[Int] = {
    val bingo = readBingoGame
    bingo.findWinners.lastOption match {
      case Some(loser) => Some(loser.score)
      case None        => None
    }
  }
}
