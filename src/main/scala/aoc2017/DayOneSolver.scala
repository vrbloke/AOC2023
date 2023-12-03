package com.vrbloke
package aoc2017

import aocframework.AocSolver

import scala.annotation.tailrec

class DayOneSolver(inputPathname: String) extends AocSolver(inputPathname) {
  override val input: List[Int] = source.mkString.toList.map(_.toString.toInt)

  override def part1(): String =
    val tuples = input.lazyZip(input.drop(1) ++ input.take(1))
    accumulate(tuples).toString

  @tailrec
  private def accumulate(xs: Iterable[(Int, Int)], sum: Int = 0): Int =
    if xs.isEmpty then return sum
    accumulate(xs.drop(1), if xs.head(0) == xs.head(1) then sum + xs.head(0) else sum)

  override def part2(): String =
    val len = input.length
    val tuples = input.lazyZip(input.drop(len/2) ++ input.take(len/2))
    accumulate(tuples).toString
}
