package adventofcode.solutions

import adventofcode.Day

object Day18 extends Day(18):

  enum Token:
    case Literal(v: Int)
    case Plus
    case Times
    case OpenParenthesis
    case CloseParenthesis

  import Token._

  val expressions = lines.map(_.toCharArray.toIndexedSeq.filter(_ != ' ').map {
    case '(' => OpenParenthesis
    case ')' => CloseParenthesis
    case '+' => Plus
    case '*' => Times
    case d => Literal(d.asDigit)
  })

  def evaluate(precedences: Seq[Seq[Token]]): Long =
    def precedence(op: Token): Int = precedences.indexWhere(_.contains(op))
    def postfix(seq: Seq[Token], output: Seq[Token], stack: Seq[Token]): Seq[Token] =
      seq match
        case h +: tail =>
          h match
            case _: Literal => postfix(tail, h +: output, stack)
            case OpenParenthesis => postfix(tail, output, h +: stack)
            case CloseParenthesis =>
              val (left, right) = stack.span(_ != OpenParenthesis)
              postfix(tail, left.reverse ++ output, if right.head == OpenParenthesis then right.tail else right)
            case _ =>
              val (left, right) = stack.span(c => c != OpenParenthesis && precedence(c) >= precedence(h))
              postfix(tail, left.reverse ++ output, h +: right)
        case _ => stack.reverse ++ output
    def reduce(seq: Seq[Token]): (Long, Int) =
      seq.head match
        case Literal(v) => (v, 1)
        case op =>
          val (left, i) = reduce(seq.tail)
          val (right, j) = reduce(seq.tail.drop(i))
          val f: (Long, Long) => Long = (op: @unchecked) match
            case Plus => _ + _
            case Times => _ * _
          (f(left, right), i + j + 1)
    expressions.map(expression => reduce(postfix(expression, Seq.empty, Seq.empty))._1).sum

  override def solutionA = evaluate(Seq(Seq(Times, Plus)))

  override def solutionB = evaluate(Seq(Seq(Times), Seq(Plus)))
