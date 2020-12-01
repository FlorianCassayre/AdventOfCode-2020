package adventofcode.solutions

import adventofcode.Day

object Day01 extends Day(1):

  val goal = 2020
  val ns = lines.map(_.toInt).sorted
  
  def pair(a: Int, b: Int): Int =
    ns(a) + ns(b) match
      case `goal` => ns(a) * ns(b)
      case s if s < goal => pair(a + 1, b)
      case s if s > goal => pair(a, b - 1)
  
  override def solutionA = pair(0, ns.size - 1)

  def triple: Int =
    (for {
      a <- ns.indices
      b <- ns.indices.drop(a + 1)
      c <- ns.indices.drop(b + 1)
      vs = Seq(a, b, c).map(ns)
      if vs.sum == goal
    } yield vs.product).head
  
  override def solutionB = triple

