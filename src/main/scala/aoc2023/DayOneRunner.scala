package com.vrbloke
package aoc2023

import aocframework.AocRunner

import scala.util.matching.Regex

class DayOneRunner(inputPathname: String) extends AocRunner(inputPathname) {
  override def input: List[String] = source.mkString.split("\n").toList

  private def digitsR: Regex = raw"(zero|one|two|three|four|five|six|seven|eight|nine)".r

  /**
   * <p/>The newly-improved calibration document consists of lines of text; each line originally contained a specific calibration value that the Elves now need to recover. On each line, the calibration value can be found by combining the first digit and the last digit (in that order) to form a single two-digit number.
   * <p/>For example:
   * <pre>
   * 1abc2
   * pqr3stu8vwx
   * a1b2c3d4e5f
   * treb7uchet
   * </pre>
   * <p/>In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces 142.
   * <p/>Consider your entire calibration document. What is the sum of all of the calibration values?
   * */
  override def part1(): String =
    input.map(
      _.filter(_.isDigit).headAndTail.map(_.toString).foldRight("")(_++_).toInt
    ).sum.toString

  /**
   * <p/>Your calculation isn't quite right. It looks like some of the digits are actually spelled out with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".
   * <p/>Equipped with this new information, you now need to find the real first and last digit on each line. For example:
   * <pre>
   * two1nine
   * eightwothree
   * abcone2threexyz
   * xtwone3four
   * 4nineeightseven2
   * zoneight234
   * 7pqrstsixteen
   * </pre>
   * <p/>In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces 281.
   * <p/>What is the sum of all of the calibration values?
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
