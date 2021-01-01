package adventofcode.solutions

import adventofcode.Day

object Day23 extends Day(23):

  val initialCups = input.toSeq.map(_.asDigit)

  def play(turns: Int, cups: Seq[Int]): Seq[Int] =
    val size = cups.size
    val (_, ring) = (0 until turns).foldLeft((cups.head, cups.zip(cups.tail :+ cups.head).toMap)) { case ((current, ring), _) =>
      val picked = Seq.iterate(ring(current), 3)(ring)
      val stream = LazyList.iterate(current)(v => (v - 1 + ring.size - 1) % ring.size + 1).tail
      val destination = stream.dropWhile(picked.contains).head
      val newRing = ring ++ Map(current -> ring(picked.last), destination -> picked.head, picked.last -> ring(destination))
      val newCurrent = newRing(current)
      (newCurrent, newRing)
    }
    Seq.iterate(ring(1), size - 1)(ring)

  override def solutionA = play(100, initialCups).mkString

  override def solutionB = play(10000000, initialCups ++ (initialCups.max + 1 to 1000000)).take(2).map(_.toLong).product
