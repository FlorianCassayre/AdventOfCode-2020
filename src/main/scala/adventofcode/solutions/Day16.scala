package adventofcode.solutions

import adventofcode.Day

object Day16 extends Day(16):

  val parts = input.split(lineSeparator + lineSeparator).map(_.split(lineSeparator))

  val regexRanges = "([a-z ]+): ([0-9]+)-([0-9]+) or ([0-9]+)-([0-9]+)".r

  val (departures, constraints) = parts(0).map {
    case regexRanges(name, a0, b0, a1, b1) => name.startsWith("departure") -> Seq(a0.toInt to b0.toInt, a1.toInt to b1.toInt)
  }.unzip
  val ownTicket = parts(1).tail.head.split(",").map(_.toInt)
  val otherTickets = parts(2).tail.map(_.split(",").map(_.toInt))

  override def solutionA = otherTickets.flatten.filter(v => constraints.flatten.forall(!_.contains(v))).sum

  val validTickets = ownTicket +: otherTickets.filter(!_.exists(v => constraints.flatten.forall(!_.contains(v))))

  val matrix = ownTicket.indices.map(i => ownTicket.indices.map(j => validTickets.forall(ticket => constraints(j).exists(_.contains(ticket(i))))))

  def findSat(assignment: Map[Int, Int], leftIndices: Set[Int], leftValues: Set[Int]): Option[Map[Int, Int]] =
    if leftIndices.isEmpty then
      Some(assignment)
    else
      val cross = leftIndices.toSeq.sortBy(i => -matrix(i).count(identity)).flatMap(i => leftValues.toSeq.map(j => (i, j)))
      val crossFiltered = cross.filter((i, j) => matrix(i)(j))
      crossFiltered.map((i, j) => findSat(assignment + (i -> j), leftIndices - i, leftValues - j)).find(_.nonEmpty).flatten

  val indicesSet = ownTicket.indices.toSet

  def matching(u: Int, seen: Set[Int], matched: Map[Int, Int]): Option[(Set[Int], Map[Int, Int])] =
    def iterate(indices: Seq[Int], seen: Set[Int], matched: Map[Int, Int]): Option[(Set[Int], Map[Int, Int])] =
      indices match
        case Seq() => None
        case v +: tail =>
          if matrix(u)(v) && !seen.contains(v) then
            val newSeen = seen + v
            if !matched.contains(v) then
              Some(newSeen, matched + (v -> u))
            else
              matching(matched(v), newSeen, matched) match
                case Some(newSeen1, newMatched) => Some(newSeen1, newMatched + (v -> u))
                case None => iterate(indices.tail, newSeen, matched)
          else
            iterate(indices.tail, seen, matched)
    iterate(ownTicket.indices, seen, matched)

  def maxMatching(indices: Seq[Int], matched: Map[Int, Int]): Map[Int, Int] =
    indices match 
      case u +: tail => maxMatching(tail, matching(u, Set.empty, matched).get._2)
      case Seq() => matched

  override def solutionB = maxMatching(ownTicket.indices, Map.empty).toIndexedSeq.filter(t => departures(t._1)).map(t => ownTicket(t._2).toLong).product
