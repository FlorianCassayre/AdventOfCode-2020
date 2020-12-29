package adventofcode.solutions

import adventofcode.Day

object Day02 extends Day(2):

  val regex = "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)".r

  val parsed = lines.map {
    case regex(minStr, maxStr, char, str) =>
      (minStr.toInt, maxStr.toInt, char.head, str)
  }

  override def solutionA = parsed.count { (min, max, char, str) =>
    val c = str.count(_ == char)
    min <= c && c <= max
  }

  override def solutionB = parsed.count((min, max, char, str) =>
    str(min - 1) == char ^ str(max - 1) == char
  )
