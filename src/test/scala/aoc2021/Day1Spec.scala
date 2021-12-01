package aoc2021

import org.scalatest.flatspec.AnyFlatSpec

class Day1Spec extends AnyFlatSpec {
  behavior of "partOne"

  it should "return the correct result" in {
    assert(Day1.partOne === 1759)
  }
}
