package com.vrbloke
package aocframework

import scala.io.BufferedSource

abstract class AocSolver(inputPathname: String) {
  val source: BufferedSource = io.Source.fromFile(inputPathname)

  /** Solve part 1 of the puzzle */
  def part1(): String

  /** Solve part 2 of the puzzle */
  def part2(): String
}
