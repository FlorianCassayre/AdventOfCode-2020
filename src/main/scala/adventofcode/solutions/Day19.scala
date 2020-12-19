package adventofcode.solutions

import adventofcode.Day

object Day19 extends Day(19):

  val parts = input.split(lineSeparator + lineSeparator).toVector.map(_.split(lineSeparator).toVector)

  enum Case:
    case Literal(c: Char)
    case Pipe(rules: Seq[Seq[Int]])

  import Case._

  val rules = parts.head.map { case s"$id: $rule" =>
    val cse = rule match
      case s""""$c"""" => Literal(c.head)
      case other => Pipe(other.split(" \\| ").toSeq.map(_.split(" ").toSeq.map(_.toInt)))
    id.toInt -> cse
  }.toMap

  val messages = parts.tail.head

  val max = messages.map(_.size).max

  val updatedRules  = rules + (8 -> Pipe(Seq(Seq(42), Seq(42, 8)))) + (11 -> Pipe(Seq(Seq(42, 31), Seq(42, 11, 31))))

  def build(rules: Map[Int, Case], id: Int, depth: Int): String =
    if depth >= max then
      ""
    else
      rules(id) match
        case Literal(c) => c.toString
        case Pipe(rule) =>
          val cases = rule.map(_.map(s => build(rules, s, depth + 1)).mkString).mkString("|")
          s"(?:$cases)"

  val regex1 = build(rules, 0, 0).r

  override def solutionA = messages.count(s => regex1.matches(s))

  val regex2 = build(updatedRules, 0, 0).r

  override def solutionB = messages.count(s => regex2.matches(s))
