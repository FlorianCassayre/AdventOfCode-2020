package adventofcode.solutions

import adventofcode.Day

import scala.language.implicitConversions

object Day10 extends Day(10):

  val parsed = lines.map(_.toInt)
  val sorted = 0 +: parsed.sorted :+ (parsed.max + 3)

  val differences = sorted.tail.zip(sorted).map(_ - _)

  override def solutionA = differences.count(_ == 1) * differences.count(_ == 3)

  lazy val sequence: LazyList[Long] = 0L #:: 1L #:: 1L #:: sequence.sliding(3).to(LazyList).map(_.sum)

  override def solutionB = differences
    .foldLeft(Seq(1))((acc, e) => e match
      case 1 => (acc.head + 1) +: acc.tail
      case 3 => 1 +: acc
    ).map(sequence).product
