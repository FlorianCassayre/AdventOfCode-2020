package adventofcode.solutions

import adventofcode.Day

object Day05 extends Day(5):
  
  def parse(`0`: Char, `1`: Char)(code: String): Int =
    code.map {
      case `0` => 0
      case `1` => 1
    }.foldLeft(0)(_ << 1 | _)

  val seats = lines
    .map(_.splitAt(7))
    .map((r, c) => parse('F', 'B')(r) * 8 + parse('L', 'R')(c))
    .sorted

  override def solutionA = seats.max
  
  override def solutionB = seats.tail.zip(seats).find(_ - _ == 2).map(_._1 - 1).get
