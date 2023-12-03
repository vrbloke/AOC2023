package com.vrbloke

import aocframework.AocSolver

@main
def AocExecution(year: String, day: Int, part: Int): Unit = {
  val runner: AocSolver = Class.forName(s"com.vrbloke.aoc$year.Day${mapNumberToWord(day, true)}Solver")
    .getDeclaredConstructor(classOf[String])
    .newInstance(s"$year-$day.txt")
    .asInstanceOf[AocSolver]

  println(s"Running Day ${mapNumberToWord(day)} Part ${mapNumberToWord(part)}!")
  part match {
    case 1 => runAndTime(runner.part1)
    case 2 => runAndTime(runner.part2)
    case 3 => runAndTime(runBoth(runner))
  }
}

def runBoth(r: AocSolver): () => String =
  () => {
    println(r.part1())
    println(r.part2())
    "Timing both parts..."
  }

def runAndTime(f: () => String): Unit = {
  val start = System.nanoTime()
  println(f())
  val end = System.nanoTime()
  val elapsed: Double = end - start
  println(f"Execution took ${elapsed * 1e-9}%.2f\ufeffs, or ${elapsed * 1e-6}%.3f\ufeffms")
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