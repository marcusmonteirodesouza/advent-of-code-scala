package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day1 {
  private def readSonarSweepReport: List[Int] =
    Source.fromResource("aoc2021/day-1.txt").getLines().map(_.toInt).toList

  def partOne: Int = {
    @tailrec
    def partOneGo(sonarSweepReport: List[Int],
                  numberOfLargerMeasurements: Int): Int = {
      sonarSweepReport match {
        case previousMeasurement :: currentMeasurement :: rest =>
          if (currentMeasurement > previousMeasurement) {
            partOneGo(currentMeasurement :: rest,
                      numberOfLargerMeasurements + 1)
          } else {
            partOneGo(currentMeasurement :: rest, numberOfLargerMeasurements)
          }
        case _ => numberOfLargerMeasurements

      }
    }

    val sonarSweepReport = readSonarSweepReport

    partOneGo(sonarSweepReport, 0)
  }

  def partTwo: Int = {
    @tailrec
    def partTwoGo(sonarSweepReport: List[Int],
                  numberOfLargerMeasurements: Int): Int = {
      sonarSweepReport match {
        case prev1 :: prev2 :: prev3 :: curr :: rest =>
          if (prev2 + prev3 + curr > prev1 + prev2 + prev3) {
            partTwoGo(prev2 :: prev3 :: curr :: rest,
                      numberOfLargerMeasurements + 1)
          } else {
            partTwoGo(prev2 :: prev3 :: curr :: rest,
                      numberOfLargerMeasurements)
          }
        case _ => numberOfLargerMeasurements
      }
    }

    val sonarSweepReport = readSonarSweepReport

    partTwoGo(sonarSweepReport, 0)
  }
}
