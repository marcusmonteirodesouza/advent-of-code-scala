package aoc2021

import org.scalatest.flatspec.AnyFlatSpec

class Day3Spec extends AnyFlatSpec {
  behavior of "partOne"

  it should "return the correct result" in {
    assert(Day3.partOne === 3309596)
  }

  behavior of "partTwo"

  it should "return the correct result" in {
    assert(Day3.partTwo === 2981085)
  }
}
