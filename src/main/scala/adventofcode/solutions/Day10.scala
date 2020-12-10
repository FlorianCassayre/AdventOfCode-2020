package adventofcode.solutions

import adventofcode.Day

import scala.util.chaining._
import scala.language.implicitConversions

object Day10 extends Day(10):

  val parsed = lines.map(_.toInt)
  val sorted = 0 +: parsed.sorted :+ (parsed.max + 3)

  val differences = sorted.tail.zip(sorted).map(_ - _)

  override def solutionA = differences.count(_ == 1) * differences.count(_ == 3)

  val sequence = LazyList.unfold(Seq(1L, 1L, 2L))(s => Some(s.head, s.tail :+ s.sum))

  override def solutionB = Seq.unfold(differences) {
    case s@(h +: _) => s.span(_ == h).pipe((l, r) => Some(l.count(_ == 1), r))
    case _ => None
  }.map(sequence).product
