package com.vrbloke
package aoc2017

import aocframework.AocRunner

import scala.annotation.tailrec

class DayOneRunner(inputPathname: String) extends AocRunner(inputPathname) {
  override val input: List[Int] = source.mkString.toList.map(_.toString.toInt)

  override def part1(): String =
    accumulate(input).toString

  @tailrec
  private def accumulate(xs: List[Int], sum: Int = 0): Int =
    if xs.tail.isEmpty then return sum + (if xs.head == input.head then xs.head else 0)
    if xs.head == xs.tail.head then accumulate(xs.tail, sum + xs.head) else accumulate(xs.tail, sum)

  override def part2(): String =
    val len = input.length
    var sum = 0
    for i <- 0 until len do
      if input(i) == input((i + len/2) % len) then sum = sum + input(i)
    sum.toString
}
