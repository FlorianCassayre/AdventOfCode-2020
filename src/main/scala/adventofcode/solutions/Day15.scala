package adventofcode.solutions

import adventofcode.Day

object Day15 extends Day(15):

  val numbers = input.split(",").map(_.toInt)

  def play(total: Int)(i: Int = numbers.size - 1, last: Int = numbers.last, map: Map[Int, Int] = numbers.init.zipWithIndex.toMap): Int =
    if i == total - 1 then last else play(total)(i + 1, i - map.getOrElse(last, i), map + (last -> i))

  override def solutionA = play(2020)()

  override def solutionB = play(30000000)()
