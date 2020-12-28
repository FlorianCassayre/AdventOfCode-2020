package adventofcode.solutions

import adventofcode.Day

import scala.language.implicitConversions
import scala.util.chaining._

object Day23 extends Day(23):

  val initialCups = input.toSeq.map(_.asDigit)
  val initialFirstCup = initialCups.head

  case class Ring(size: Int, left: Map[Int, Int], right: Map[Int, Int]):
    def picked(current: Int): Seq[Int] = Seq.unfold((current, 3))((cup, i) => if i > 0 then right(cup).pipe(r => Some(r, (r, i - 1))) else None)
    def link(a: Int, b: Int): Ring = copy(left = left + (b -> a), right = right + (a -> b))
    def sequence: Seq[Int] =
      val `1` = 1
      Seq.unfold(`1`)(cup => right(cup).pipe(r => if r != `1` then Some(r, r) else None))

  object Ring:
    def apply(cups: Seq[Int]): Ring =
      val plusOne = cups :+ cups.head
      Ring(cups.size, plusOne.tail.zip(plusOne).toMap, plusOne.zip(plusOne.tail).toMap)

  def play(turn: Int, current: Int, ring: Ring): Ring =
    if turn > 0 then
      val stream = LazyList.unfold(current)(l => (l - 1).pipe(v => if v != 0 then v else ring.size).pipe(v => Some(l, v))).tail
      val picked = ring.picked(current)
      val destination = stream.dropWhile(picked.contains).head
      val newRing = ring.link(current, ring.right(picked.last)).link(destination, picked.head).link(picked.last, ring.right(destination))
      val newCurrent = newRing.right(current)
      play(turn - 1, newCurrent, newRing)
    else
      ring

  override def solutionA = play(100, initialFirstCup, Ring(initialCups)).sequence.mkString

  override def solutionB = play(10000000, initialFirstCup, Ring(initialCups ++ (initialCups.max to 1000000).tail)).sequence.take(2).map(_.toLong).product
