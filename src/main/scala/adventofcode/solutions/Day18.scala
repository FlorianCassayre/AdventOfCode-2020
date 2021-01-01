package adventofcode.solutions

import adventofcode.Day

object Day18 extends Day(18):

  enum Operation(val apply: (Long, Long) => Long):
    case Addition extends Operation(_ + _)
    case Multiplication extends Operation(_ * _)

  import Operation._

  enum Token:
    case Literal(v: Int)
    case Operator(op: Operation)
    case OpenParenthesis
    case CloseParenthesis

  import Token._

  val expressions = lines.map(_.filter(_ != ' ').map {
    case '(' => OpenParenthesis
    case ')' => CloseParenthesis
    case '+' => Operator(Addition)
    case '*' => Operator(Multiplication)
    case d => Literal(d.asDigit)
  })

  def compute(precedences: Seq[Seq[Operation]]): Long =
    def precedence(op: Operation): Int = precedences.indexWhere(_.contains(op))
    def reduce(values: Seq[Long], operations: Seq[Operation]): Seq[Long] =
      operations.foldLeft(values)((acc, op) => op.apply(acc(1), acc(0)) +: acc.drop(2))
    def evaluate(tokens: Seq[Token]): Long =
      val (output, stack) = tokens.foldLeft((Seq.empty[Long], Seq(Seq.empty[Operation]))) { case ((output, stack), token) =>
        token match
          case Literal(v) => (v +: output, stack)
          case OpenParenthesis => (output, Seq.empty +: stack)
          case CloseParenthesis => (reduce(output, stack.head), stack.tail)
          case Operator(op) =>
            val (left, right) = stack.head.span(precedence(_) >= precedence(op))
            (reduce(output, left), (op +: right) +: stack.tail)
      }
      reduce(output, stack.head).head
    expressions.map(evaluate).sum

  override def solutionA = compute(Seq(Seq(Addition, Multiplication)))

  override def solutionB = compute(Seq(Seq(Multiplication), Seq(Addition)))
