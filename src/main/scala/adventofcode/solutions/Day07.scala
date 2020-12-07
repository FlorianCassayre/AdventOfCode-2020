package adventofcode.solutions

import adventofcode.Day

object Day07 extends Day(7):

  val rPrefix = "(.+) bags contain (?:(no other bags)|(.*))\\.".r
  val rSeq = "([0-9]+) (.+) bags?".r

  val ShinyGold = "shiny gold"

  val graph = lines.map {
    case rPrefix(bag1, _, rest) =>
      val children = Option(rest) match {
        case Some(_) =>
          rest.split(", ").map {
            case rSeq(count, bag2) => (bag2, count.toInt)
          }.toSeq
        case None => Seq()
      }
      (bag1, children)
  }.toMap

  def getColors(parent: String, color: String): Set[String] =
    if (color == ShinyGold)
      Set(parent)
    else
      graph(color).map(_._1).flatMap(s => getColors(parent, s)).toSet

  override def solutionA = graph.keys.filter(_ != ShinyGold).flatMap(s => getColors(s, s)).toSet.size

  def countBags(color: String): Int = graph(color).map((other, count) => count * countBags(other)).sum + 1

  override def solutionB = countBags(ShinyGold) - 1
