package aoc2021

import org.scalatest.flatspec.AnyFlatSpec

class Day2Spec extends AnyFlatSpec {
  behavior of "partOne"

  it should "return the correct result" in {
    assert(Day2.partOne === 2091984)
  }

  behavior of "partTwo"

  it should "return the correct result" in {
    assert(Day2.partTwo === 2086261056)
  }
}
