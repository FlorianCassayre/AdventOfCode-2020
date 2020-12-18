package adventofcode.solutions

import adventofcode.Day

import scala.util.Try

object Day18 extends Day(18):

  val (plus, times) = ('+', '*')
  
  val operations: Map[Char, (Long, Long) => Long] = Map(plus -> (_ + _), times -> (_ * _))

  val allTokens = lines.map(_.replace(" ", "").toCharArray.toIndexedSeq)

  def evaluate(tokens: Seq[Char], precedences: Seq[Seq[Char]]): Long =
    def precedence(op: Char): Int = precedences.indexWhere(_.contains(op))
    def postfix(seq: Seq[Char], output: Seq[Char], stack: Seq[Char]): Seq[Char] =
      seq match
        case h +: tail =>
          h match
            case _ if h.isDigit => postfix(tail, h +: output, stack)
            case '(' =>
              postfix(tail, output, h +: stack)
            case ')' =>
              val (left, right) = stack.span(_ != '(')
              val newOutput = left.reverse ++ output
              val newStack1 = if right.head == '(' then right.tail else right
              postfix(tail, newOutput, newStack1)
            case _ =>
              val (left, right) = stack.span(c => c != '(' && precedence(c) >= precedence(h))
              postfix(tail, left.reverse ++ output, h +: right)
        case _ => stack.reverse ++ output
    def reduce(seq: Seq[Char]): (Long, Int) =
      seq.head match
        case h if h.isDigit => (seq.head.asDigit, 1)
        case op =>
          val (left, i) = reduce(seq.tail)
          val (right, j) = reduce(seq.tail.drop(i))
          (operations(op)(left, right), i + j + 1)
    reduce(postfix(tokens, Seq.empty, Seq.empty))._1

  override def solutionA = allTokens.map(tokens => evaluate(tokens, Seq(Seq(times, plus)))).sum

  override def solutionB = allTokens.map(tokens => evaluate(tokens, Seq(Seq(times), Seq(plus)))).sum
