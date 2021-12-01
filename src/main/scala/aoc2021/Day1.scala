package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day1 {
  def partOne: Int = {
    @tailrec
    def partOneGo(sonarSweepReport: List[Int],
                  numberOfLargerMeasurements: Int): Int = {
      sonarSweepReport match {
        case Nil      => numberOfLargerMeasurements
        case _ :: Nil => numberOfLargerMeasurements
        case previousMeasurement :: currentMeasurement :: rest =>
          if (currentMeasurement > previousMeasurement) {
            partOneGo(currentMeasurement :: rest,
                      numberOfLargerMeasurements + 1)
          } else {
            partOneGo(currentMeasurement :: rest, numberOfLargerMeasurements)
          }
      }
    }

    val sonarSweepReport =
      Source.fromResource("aoc2021/day-1.txt").getLines().map(_.toInt).toList

    partOneGo(sonarSweepReport, 0)
  }
}
