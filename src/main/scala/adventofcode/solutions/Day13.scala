package adventofcode.solutions

import adventofcode.Day

import scala.util.chaining._
import scala.language.implicitConversions

object Day13 extends Day(13):

  val timestamp = lines.head.toInt
  val buses = lines.tail.head.split(",").toIndexedSeq.zipWithIndex
    .filter((b, _) => b != "x")
    .map((b, i) => (b.toLong, i.toLong))

  def remainders(na: Seq[(Long, Long)]): Long =
    val product = na.map(_._1).product
    def iterate(na: Seq[(Long, Long)], acc: Long): Long =
      def inverse(a: Long, b: Long): Long =
        def euclidean(a: Long, b: Long, x: Long, y: Long): Long =
          if a > 1 then euclidean(b, a % b, y - (a / b) * x, x) else y
        if b == 1 then 1 else euclidean(a, b, 0, 1).pipe(x => if (x < 0) x + b else x)
      na match
        case (hn, ha) +: t => (product / hn).pipe(p => iterate(t, acc + ha * inverse(p, hn) * p))
        case _ => acc
    iterate(na, 0) % product

  override def solutionA = buses.map(_._1).map(b => (b, b * (timestamp / b + 1))).minBy((_, t) => t).pipe((b, t) => b * (t - timestamp))

  override def solutionB = remainders(buses.map((b, i) => (b, (b - i))))
