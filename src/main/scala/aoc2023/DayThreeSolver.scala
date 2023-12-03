package com.vrbloke
package aoc2023

import aocframework.AocSolver

import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer
/**
 * See puzzle at https://adventofcode.com/2023/day/3
 */
class DayThreeSolver(inputPathname: String) extends AocSolver(inputPathname) {
  private def envelope(v: Vector[Char], s: Int, e: Int): ArrayBuffer[Char] =
    ArrayBuffer.from(v.slice(s-1-rowLen, e+1-rowLen) ++ Vector(v(s-1), v(e)) ++ v.slice(s-1+rowLen, e+1+rowLen))
  private def envelope2schem(i: Int, s: Int): Int = i match
      case i if i <= 2 => s - rowLen - 1 + i
      case i if i == 3 => s - 1
      case i if i == 4 => s + 1
      case i if i > 4 => s + rowLen - 6 + i

  private val input: String = source.mkString
  private def rowLen: Int = input.indexWhere(_ == '\n')+2
  private val schem: Vector[Char] = ("O"*(rowLen-1)+"\nO" + input.replaceAll("\n", "\nO") + "\n"+"O"*(rowLen)).toVector

  private def isSymbol(c: Char): Boolean = !(c.isDigit || c == '.' || c == '\n' || c == 'O')
  private def isIgnored(c: Char): Boolean = c == '\n' || c == 'O'

  override def part1(): String =
    parseForNumbers(0).toString

  @tailrec
  private def parseForNumbers(i: Int, sum: Int = 0): Int =
    if i >= schem.length then sum else schem(i) match
    case c if c.isDigit =>
      val end = schem.indexWhere(c => !c.isDigit, i)
      envelope(schem, i, end) match
        case v if v.exists(isSymbol) => parseForNumbers(end+1, sum + schem.slice(i, end).mkString.toInt)
        case _ => parseForNumbers(end+1, sum)
    case _ => parseForNumbers(i+1, sum)

  override def part2(): String =
    parseForStars(0).toString

  private def readNumber(envInd: Int, numInd: Int, envelope: ArrayBuffer[Char]): Int =
    val s = numInd - schem.slice(0,numInd+1).reverseIterator.indexWhere(c => !c.isDigit, 0) + 1
    val e = schem.indexWhere(c => !c.isDigit, numInd)
    val schem2envelope = Map.from(envelope.indices.map(i => (envelope2schem(i, envInd), i)))
    val ret = schem.slice(s, e).mkString.toInt
    (s to e).foreach { i => if schem2envelope.contains(i) then envelope.update(schem2envelope(i), 'O') }  //Ignore repeats
    ret

  @tailrec
  private def parseEnvelope(envelope: ArrayBuffer[Char], i: Int, envInd: Int): Option[Int] =
    if i >= envelope.length then None else envelope(i) match
      case c if c.isDigit =>
        Some(readNumber(envInd, envelope2schem(i, envInd), envelope))
      case _ => parseEnvelope(envelope, i+1, envInd)

  @tailrec
  private def parseForStars(i: Int, sum: Int = 0): Int =
    if i >= schem.length then sum else schem(i) match
      case c if c == '*' =>
        val env = envelope(schem, i, i+1)
        parseForStars(i+1, sum + parseEnvelope(env, 0, i).getOrElse(0) * parseEnvelope(env, 0, i).getOrElse(0))
      case _ => parseForStars(i+1, sum)

}
