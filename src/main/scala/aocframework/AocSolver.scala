package com.vrbloke
package aocframework

import scala.io.BufferedSource

abstract class AocSolver(inputPathname: String) {
  val source: BufferedSource = io.Source.fromFile(inputPathname)
  
  /** This field must be initialized. Write additional methods if necessary but try not to */
  def input: Seq[?]

  /** Solve part 1 of the puzzle */
  def part1(): String

  /** Solve part 2 of the puzzle */
  def part2(): String
  
  // Debugging
  /** Print inputs */
  def printInputs(sep: String): Unit = println(input.mkString(sep))
}
