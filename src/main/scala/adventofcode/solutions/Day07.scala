package adventofcode.solutions

import adventofcode.Day

object Day07 extends Day(7):

  val rPrefix = "(.+) bags contain (?:(no other bags)|(.*))\\.".r
  val rSeq = "([0-9]+) (.+) bags?".r

  val ShinyGold = "shiny gold"

  val graph = lines.map {
    case rPrefix(bag1, _, rest) =>
      val children = Option(rest) match
        case Some(_) =>
          rest.split(", ").map {
            case rSeq(count, bag2) => (bag2, count.toInt)
          }.toSeq
        case None => Seq()
      (bag1, children)
  }.toMap

  def contains(bag: String): Boolean = bag == ShinyGold || graph(bag).exists((other, _) => contains(other))

  override def solutionA = graph.keys.filter(_ != ShinyGold).count(contains)

  def countBags(bag: String): Int = graph(bag).map((other, count) => count * countBags(other)).sum + 1

  override def solutionB = countBags(ShinyGold) - 1
