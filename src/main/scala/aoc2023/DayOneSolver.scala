package com.vrbloke
package aoc2023

import aocframework.AocSolver

import scala.util.matching.Regex

/**
 * See puzzle at https://adventofcode.com/2023/day/1
 */
class DayOneSolver(inputPathname: String) extends AocSolver(inputPathname) {
  def input: List[String] = source.mkString.split("\n").toList

  private def digitsR: Regex = raw"(zero|one|two|three|four|five|six|seven|eight|nine)".r
  
  override def part1(): String =
    input.map(
      _.filter(_.isDigit).headAndTail.map(_.toString).foldRight("")(_++_).toInt
    ).sum.toString

  /**
   * For part 2, some of the numbers can be represented by words.
   */
  override def part2(): String =
    input
      .map(row =>
        val doubledEndings = digitsR.replaceAllIn(row, m => m.toString() + m.toString().last)
        digitsR.replaceAllIn(doubledEndings, m => mapToDigit(m.toString()))  // Map words to digits
      )
      .map(_.filter(_.isDigit).headAndTail.map(_.toString).foldRight("")(_++_).toInt) // Filter out non-digits and get value
      .sum.toString

  private def mapToDigit(s: String): String = s match
    case "zero" => "0"
    case "one" => "1"
    case "two" => "2"
    case "three" => "3"
    case "four" => "4"
    case "five" => "5"
    case "six" => "6"
    case "seven" => "7"
    case "eight" => "8"
    case "nine" => "9"

  extension (s: String)
    private def headAndTail: List[Char] = List(s.head, s.last)
}
