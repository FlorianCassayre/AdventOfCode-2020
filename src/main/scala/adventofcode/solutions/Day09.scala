package adventofcode.solutions

import adventofcode.Day

object Day09 extends Day(9):

  val window = 25
  val numbers = lines.map(BigInt.apply)

  val outlier = numbers.drop(window).zip(numbers.sliding(window))
    .find((n, p) => p.combinations(2).forall(_.sum != n)).get._1

  override def solutionA = outlier

  override def solutionB =
    (for
    i <- numbers.indices
    j <- numbers.indices.drop(i + 2)
    slice = numbers.slice(i, j)
    if slice.sum == outlier
      yield slice.min + slice.max).head
