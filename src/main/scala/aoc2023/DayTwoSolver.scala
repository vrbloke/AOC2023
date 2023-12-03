package com.vrbloke
package aoc2023

import aocframework.AocSolver

import Ordering.by

/**
 * See puzzle at https://adventofcode.com/2023/day/2
 */
class DayTwoSolver(inputPathname: String) extends AocSolver(inputPathname) {
  protected case class TurnRecord(red: Int, blue: Int, green: Int)
  protected case class GameRecord(id: Int, turns: List[TurnRecord])

  def input: List[GameRecord] =
    source.mkString.split("\n").map(_.split(";")).zipWithIndex.map(parseGameRecord).toList

  override def part1(): String =
    val (redCap, greenCap, blueCap) = (12, 13, 14)
    input
      .filter(gr => gr.turns.forall(t => t.red <= redCap && t.green <= greenCap && t.blue <= blueCap))
      .map(_.id)
      .sum.toString

  /**
   * For part 2, you must find the maximum number of cubes of each color used in a game
   * and calculate the product of the three maxima.
   */
  override def part2(): String =
    input
      .map(maxCubesShown)
      .map(t => t._1 * t._2 * t._3)
      .sum.toString

  private def parseTurnRecord(s: String): TurnRecord =
    var (red, green, blue) = (0, 0, 0)
    "([0-9]+) ([a-z]+)".r.findAllMatchIn(s).map(_.subgroups).foreach{
      case List(n, "red") => red = n.toInt
      case List(n, "green") => green = n.toInt
      case List(n, "blue") => blue = n.toInt
      case _ => throw IllegalArgumentException("Not a turn record")
    }
    TurnRecord(red, blue, green)

  private def parseGameRecord(t: (Array[String], Int)): GameRecord = t match
    case (turns, id) => GameRecord(id+1, turns.map(parseTurnRecord).toList)

  private def maxCubesShown(gr: GameRecord): (Int, Int, Int) = gr match
    case GameRecord(_, trs) => (trs.max(by(_.red)).red, trs.max(by(_.green)).green, trs.max(by(_.blue)).blue)
}