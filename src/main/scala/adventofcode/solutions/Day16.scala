package adventofcode.solutions

import adventofcode.Day

object Day16 extends Day(16):

  val parts = input.split(lineSeparator * 2).map(_.split(lineSeparator).toIndexedSeq).toIndexedSeq

  val (departures, constraints) = parts.head.map {
    case s"$name: $a0-$b0 or $a1-$b1" => name.startsWith("departure") -> Seq(a0.toInt to b0.toInt, a1.toInt to b1.toInt)
  }.unzip
  val Seq(Seq(ownTicket), otherTickets) = parts.tail.map(_.tail.map(_.split(",").toIndexedSeq.map(_.toInt)))

  val impossible = otherTickets.map(ticket => ticket -> ticket.filter(v => !constraints.flatten.exists(_.contains(v))))

  override def solutionA = impossible.flatMap((_, r) => r).sum

  val validTickets = impossible.collect { case (ticket, Seq()) => ticket }
  val indices = ownTicket.indices
  val matrix = indices.map(i => indices.map(j => validTickets.forall(ticket => constraints(j).exists(_.contains(ticket(i))))))

  def maxMatching(nodes: Seq[Int], matched: Map[Int, Int]): Map[Int, Int] =
    def matching(u: Int, seen: Set[Int], matched: Map[Int, Int]): Option[(Set[Int], Map[Int, Int])] =
      def iterate(nodes: Seq[Int], seen: Set[Int], matched: Map[Int, Int]): Option[(Set[Int], Map[Int, Int])] =
        nodes match
          case v +: tail =>
            if matrix(u)(v) && !seen.contains(v) then
              val newSeen = seen + v
              if !matched.contains(v) then
                Some(newSeen, matched + (v -> u))
              else
                matching(matched(v), newSeen, matched) match
                  case Some(newSeen1, newMatched) => Some(newSeen1, newMatched + (v -> u))
                  case None => iterate(tail, newSeen, matched)
            else
              iterate(tail, seen, matched)
          case _ => None
      iterate(indices, seen, matched)
    nodes match
      case u +: tail => maxMatching(tail, matching(u, Set.empty, matched).get._2)
      case _ => matched

  override def solutionB = maxMatching(indices, Map.empty).view.filterKeys(departures).values.map(ownTicket).map(_.toLong).product
