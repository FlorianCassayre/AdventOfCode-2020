package adventofcode.solutions

import adventofcode.Day

object Day06 extends Day(6):

  val groups = input.split(lineSeparator * 2).map(_.split(lineSeparator))

  override def solutionA = groups.map(_.flatten.distinct.size).sum

  override def solutionB = groups.map(_.map(_.toSet).reduce(_ & _).size).sum
