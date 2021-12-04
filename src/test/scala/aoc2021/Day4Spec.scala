package aoc2021

import org.scalatest.flatspec.AnyFlatSpec

class Day4Spec extends AnyFlatSpec {
  behavior of "partOne"

  it should "return the correct result" in {
    assert(Day4.partOne === Some(31424))
  }

  behavior of "partTwo"

  it should "return the correct result" in {
    assert(Day4.partTwo === Some(23042))
  }
}
