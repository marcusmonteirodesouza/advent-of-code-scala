package aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day3 {
  type DiagnosticReport = Seq[String]

  private def readDiagnosticReport: DiagnosticReport =
    Source.fromResource("aoc2021/day-3.txt").getLines().toSeq

  def partOne: Int = {
    type GammaRate = Int
    type EpsilonRate = Int

    def calculateRates(
        diagnosticReport: DiagnosticReport): (GammaRate, EpsilonRate) = {
      @tailrec
      def calculateRatesGo(diagnosticReport: DiagnosticReport,
                           counter: Seq[Int]): (GammaRate, EpsilonRate) = {
        diagnosticReport match {
          case Nil =>
            def getMostCommon(count: Int) = if (count > 0) "1" else "0"

            def getLeastCommon(count: Int) = if (count < 0) "1" else "0"

            val gammaRate = Integer.parseInt(
              counter.map(count => getMostCommon(count)).mkString(""),
              2)
            val epsilonRate = Integer.parseInt(
              counter.map(count => getLeastCommon(count)).mkString(""),
              2)
            (gammaRate, epsilonRate)

          case reportRecord :: rest =>
            val updatedCounter = reportRecord
              .split("")
              .zip(counter)
              // This is to make the `getMostCommon` and `getLeastCommon` functions in the `Nil` case work.
              .map(r => if (r._1 == "1") r._2 + 1 else r._2 - 1)
            calculateRatesGo(rest, updatedCounter.toIndexedSeq)
        }

      }
      calculateRatesGo(diagnosticReport,
                       List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

    }

    val diagnosticReport = readDiagnosticReport
    val rates = calculateRates(diagnosticReport)
    rates._1 * rates._2
  }

  def partTwo: Int = {
    type OxygenGeneratorRating = Int
    type CO2ScrubberRating = Int

    def calculateRatings(diagnosticReport: DiagnosticReport)
      : (OxygenGeneratorRating, CO2ScrubberRating) = {
      def calculateOxygenGeneratorRating(
          diagnosticReport: DiagnosticReport): OxygenGeneratorRating = {
        @tailrec
        def calculateOxygenGeneratorRatingGo(
            diagnosticReport: DiagnosticReport,
            position: Int): OxygenGeneratorRating = {
          diagnosticReport match {
            case oxygenGeneratorRecord :: Nil =>
              Integer.parseInt(oxygenGeneratorRecord, 2)

            case _ =>
              val groupedByBit = diagnosticReport
                .map(_.split(""))
                .flatMap(_(position))
                .groupBy(identity)

              val count0Bits = groupedByBit('0').size
              val count1Bits = groupedByBit('1').size

              val mostCommonBit = if (count0Bits == count1Bits) {
                '1'
              } else if (count0Bits > count1Bits) {
                '0'
              } else {
                '1'
              }

              val filteredRecords =
                diagnosticReport.filter(_(position) == mostCommonBit)

              calculateOxygenGeneratorRatingGo(filteredRecords, position + 1)
          }
        }

        calculateOxygenGeneratorRatingGo(diagnosticReport, 0)
      }

      def calculateCO2ScrubberRating(
          diagnosticReport: DiagnosticReport): CO2ScrubberRating = {
        @tailrec
        def calculateCO2ScrubberRatingGo(diagnosticReport: DiagnosticReport,
                                         position: Int): CO2ScrubberRating = {
          diagnosticReport match {
            case co2ScrubberRecord :: Nil =>
              Integer.parseInt(co2ScrubberRecord, 2)

            case _ =>
              val groupedByBit = diagnosticReport
                .map(_.split(""))
                .flatMap(_(position))
                .groupBy(identity)

              val count0Bits = groupedByBit('0').size
              val count1Bits = groupedByBit('1').size

              val leastCommonBit = if (count0Bits == count1Bits) {
                '0'
              } else if (count0Bits < count1Bits) {
                '0'
              } else {
                '1'
              }

              val filteredRecords =
                diagnosticReport.filter(_(position) == leastCommonBit)

              calculateCO2ScrubberRatingGo(filteredRecords, position + 1)
          }
        }

        calculateCO2ScrubberRatingGo(diagnosticReport, 0)
      }

      (calculateOxygenGeneratorRating(diagnosticReport),
       calculateCO2ScrubberRating(diagnosticReport))
    }

    val diagnosticReport = readDiagnosticReport
    val ratings = calculateRatings(diagnosticReport)
    ratings._1 * ratings._2
  }
}
