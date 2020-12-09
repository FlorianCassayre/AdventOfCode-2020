package adventofcode.solutions

import adventofcode.Day

import scala.collection.Searching._

object Day09 extends Day(9):

  val window = 25
  val numbers = lines.map(BigInt.apply)

  val outlier = numbers.drop(window).zip(numbers.sliding(window))
    .find((n, p) => p.combinations(2).forall(_.sum != n)).get._1

  override def solutionA = outlier

  override def solutionB =
    (for
    i <- numbers.indices
    slices = numbers.indices.drop(i + 2).view.map(numbers.slice(i, _))
    slice <- slices.map(_.sum).search(outlier) match
      case Found(idx) => Some(slices(idx))
      case _ => None
    yield slice.min + slice.max).head
