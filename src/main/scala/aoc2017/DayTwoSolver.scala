package com.vrbloke
package aoc2017

import aocframework.AocSolver

import scala.annotation.tailrec

class DayTwoSolver(inputPathname: String) extends AocSolver(inputPathname) {
  override def input: List[List[Int]] = source.mkString.split("\n")
    .map(row => row.split(raw"\s").map(_.toInt).toList)
    .toList

  override def part1(): String =
    input.map(row => row.max - row.min).sum.toString

  override def part2(): String =
    input.map(row => {
      val rev = row.sorted.reverse
      compareTailToHead(rev)
    })
      .sum.toString

  @tailrec
  private def compareTailToHead(xs: List[Int]): Int =
    if xs.isEmpty then
      throw IllegalArgumentException("No pair... :((((")
    for x <- xs.tail do
      if xs.head % x == 0 then
        val ret = xs.head / x
    compareTailToHead(xs.tail)
}
