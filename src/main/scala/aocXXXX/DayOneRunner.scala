package com.vrbloke
package aocXXXX

import aocframework.AocRunner

import scala.util.Using

/** This sample runner receives a list of commma-separated integers.
 * <br>Part 1: Return the sum of inputs
 * <br>Part 2: Return the product of inputs */
class DayOneRunner(inputPathname: String) extends AocRunner(inputPathname) {
  override val input: List[Int] = {
    val source = io.Source.fromFile(inputPathname)
    source.mkString.split(",").map(_.toInt).toList
  }

  override def part1(): String = {
    printInputs(",")
    input.sum.toString
  }

  override def part2(): String = {
    input.product.toString
  }
}
