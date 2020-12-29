package adventofcode.solutions

import adventofcode.Day

object Day23 extends Day(23):

  val initialCups = input.toSeq.map(_.asDigit)
  val initialFirstCup = initialCups.head

  case class Ring(size: Int, left: Map[Int, Int], right: Map[Int, Int]):
    def picked(current: Int): Seq[Int] = Seq.iterate(right(current), 3)(right)
    def link(a: Int, b: Int): Ring = copy(left = left + (b -> a), right = right + (a -> b))
    def sequence: Seq[Int] = Seq.iterate(right(1), size - 1)(right)

  object Ring:
    def apply(cups: Seq[Int]): Ring =
      val plusOne = cups :+ cups.head
      Ring(cups.size, plusOne.tail.zip(plusOne).toMap, plusOne.zip(plusOne.tail).toMap)

  def play(turn: Int, current: Int, ring: Ring): Ring =
    if turn > 0 then
      val stream = LazyList.iterate(current)(v => (v - 1 + ring.size - 1) % ring.size + 1).tail
      val picked = ring.picked(current)
      val destination = stream.dropWhile(picked.contains).head
      val newRing = ring.link(current, ring.right(picked.last)).link(destination, picked.head).link(picked.last, ring.right(destination))
      val newCurrent = newRing.right(current)
      play(turn - 1, newCurrent, newRing)
    else
      ring

  override def solutionA = play(100, initialFirstCup, Ring(initialCups)).sequence.mkString

  override def solutionB = play(10000000, initialFirstCup, Ring(initialCups ++ (initialCups.max to 1000000).tail)).sequence.take(2).map(_.toLong).product
