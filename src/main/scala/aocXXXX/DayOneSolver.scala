package com.vrbloke
package aocXXXX

import aocframework.AocSolver

/** This sample runner receives a list of commma-separated integers.
 * <br/>Part 1: Return the sum of inputs
 * <br/>Part 2: Return the product of inputs */
class DayOneSolver(inputPathname: String) extends AocSolver(inputPathname) {
  def input: List[Int] =
    source.mkString.split(",").map(_.toInt).toList

  override def part1(): String =
    input.sum.toString

  override def part2(): String =
    input.product.toString
}
