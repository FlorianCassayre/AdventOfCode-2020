package adventofcode.solutions

import adventofcode.Day

import scala.util.chaining._
import scala.language.implicitConversions

object Day13 extends Day(13):

  val timestamp = lines.head.toInt
  val buses = lines.tail.head.split(",").toIndexedSeq.zipWithIndex
    .filter((b, _) => b != "x")
    .map((b, i) => (b.toLong, i.toLong))

  def remainder(na: Seq[(Long, Long)]): Long =
    val product = na.map((n, _) => n).product
    def iterate(na: Seq[(Long, Long)], acc: Long): Long =
      def euclidean(a: Long, b: Long, x: Long, y: Long): Long = if a > 1 then euclidean(b, a % b, y - (a / b) * x, x) else y
      na match
        case (hn, ha) +: t => (product / hn).pipe(p => iterate(t, acc + ha * euclidean(p, hn, 0, 1).pipe(x => if x < 0 then x + hn else x) * p))
        case _ => acc
    iterate(na, 0) % product

  override def solutionA = buses.map((b, _) => (b, timestamp % b)).maxBy((_, t) => t).pipe((b, t) => b * (b - t))

  override def solutionB = remainder(buses.map((b, i) => (b, b - i)))
