package com.vrbloke

import aocframework.AocRunner

@main
def AocExecution(year: String, day: Int, part: Int): Unit = {
  val runner: AocRunner = Class.forName(s"com.vrbloke.aoc${year}.Day${mapNumberToWord(day, true)}Runner")
    .getDeclaredConstructor(classOf[String])
    .newInstance(s"$year-$day")
    .asInstanceOf[AocRunner]

  println(s"Running Day ${mapNumberToWord(day)} Part ${mapNumberToWord(part)}!")
  part match {
    case 1 => runAndTime(runner.part1)
    case 2 => runAndTime(runner.part2)
  }
}

def runAndTime(f: () => String): Unit = {
  val start = System.nanoTime()
  println(f())
  val end = System.nanoTime()
  val elapsed: Double = end - start
  println(f"Execution took ${elapsed * 0.000_000_001}%.2f\ufeffs, or ${elapsed * 0.000_001}%.3f\ufeffms, or ${elapsed}%.0f\ufeffns.")
}

def mapNumberToWord(num: Int, className: Boolean = false): String = num match {
  case 0 => ""
  case 1 => "One"
  case 2 => "Two"
  case 3 => "Three"
  case 4 => "Four"
  case 5 => "Five"
  case 6 => "Six"
  case 7 => "Seven"
  case 8 => "Eight"
  case 9 => "Nine"
  case 10 => "Ten"
  case 11 => "Eleven"
  case 12 => "Twelve"
  case 13 => "Thirteen"
  case 14 => "Fourteen"
  case 15 => "Fifteen"
  case 16 => "Sixteen"
  case 17 => "Seventeen"
  case 18 => "Eighteen"
  case 19 => "Nineteen"
  case 20 => "Twenty"
  case _ => if className then
    mapNumberToWord(num - num % 10) + mapNumberToWord(num % 10)
  else
    mapNumberToWord(num - num % 10) + "-" + mapNumberToWord(num % 10).toLowerCase
}