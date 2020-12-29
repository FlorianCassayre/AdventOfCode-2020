package adventofcode.solutions

import adventofcode.Day

object Day19 extends Day(19):

  val parts = input.split(lineSeparator * 2).map(_.split(lineSeparator))

  enum Case:
    case Literal(c: Char)
    case Pipe(rules: Seq[Seq[Int]])

  import Case._

  def parse(s: String): (Int, Case) = s match
    case s"$id: $rule" =>
      val cse = rule match
        case s""""$c"""" => Literal(c.head)
        case other => Pipe(other.split(" \\| ").toSeq.map(_.split(" ").toSeq.map(_.toInt)))
      id.toInt -> cse

  val (rules, messages) = (parts.head.map(parse).toMap, parts.tail.head)

  val max = messages.map(_.size).max

  def build(rules: Map[Int, Case], id: Int = 0, length: Int = 0): String =
    if length >= max then
      ""
    else
      rules(id) match
        case Literal(c) => c.toString
        case Pipe(rule) =>
          val cases = rule.map(r => r.map(s => build(rules, s, length + r.size)).mkString).mkString("|")
          s"($cases)"

  def count(rules: Map[Int, Case]): Int = messages.count(build(rules).r.matches)

  override def solutionA = count(rules)

  val additionalRules = """8: 42 | 42 8
                          |11: 42 31 | 42 11 31""".stripMargin.split(lineSeparator)

  override def solutionB = count(rules ++ additionalRules.map(parse))
