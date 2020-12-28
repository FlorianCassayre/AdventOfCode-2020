package adventofcode.solutions

import adventofcode.Day

object Day22 extends Day(22):

  val Seq(initialDeck1, initialDeck2) = input.split(lineSeparator + lineSeparator).map(_.split(lineSeparator).tail.map(_.toInt).toIndexedSeq).toSeq

  def score(deck: Seq[Int]): Int = deck.reverse.zipWithIndex.map((c, i) => c * (i + 1)).sum

  def playNormal(deck1: Seq[Int], deck2: Seq[Int]): Int =
    (deck1, deck2) match
      case (a +: tail1, b +: tail2) => if a > b then playNormal(tail1 :+ a :+ b, tail2) else playNormal(tail1, tail2 :+ b :+ a)
      case _ => score(deck1 ++ deck2)

  override def solutionA = playNormal(initialDeck1, initialDeck2)

  def playRecursive(deck1: Seq[Int], deck2: Seq[Int], history: Set[(Seq[Int], Seq[Int])]): (Boolean, (Seq[Int], Seq[Int])) =
    val current = ((deck1, deck2))
    if history.contains(current) then
      (true, (deck1, deck2))
    else
      val next = history + ((deck1, deck2))
      (deck1, deck2) match
        case (a +: tail1, b +: tail2) =>
          val winner = if tail1.sizeIs >= a && tail2.sizeIs >= b then playRecursive(tail1.take(a), tail2.take(b), next)._1 else a > b
          if winner then playRecursive(tail1 :+ a :+ b, tail2, next) else playRecursive(tail1, tail2 :+ b :+ a, next)
        case _ => (deck1.nonEmpty, (deck1, deck2))

  val (winner, (deck1, deck2)) = playRecursive(initialDeck1, initialDeck2, Set.empty)

  override def solutionB = score(if winner then deck1 else deck2)
