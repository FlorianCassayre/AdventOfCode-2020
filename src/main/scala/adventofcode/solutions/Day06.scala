package adventofcode.solutions

import adventofcode.Day

object Day06 extends Day(6):

  val groups = input.split(lineSeparator + lineSeparator).map(_.split(lineSeparator))

  override def solutionA = groups.map(_.flatten.toSet.size).sum

  override def solutionB = groups.map(g => g.flatten.groupBy(identity).filter(_._2.size == g.size).size).sum
