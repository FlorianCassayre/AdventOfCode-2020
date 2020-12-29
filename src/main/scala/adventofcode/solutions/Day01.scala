package adventofcode.solutions

import adventofcode.Day

import scala.collection.Searching._

object Day01 extends Day(1):

  val goal = 2020
  val ns = lines.map(_.toInt).sorted

  def pair(a: Int, b: Int): Int =
    val sum = ns(a) + ns(b)
    if sum == goal then
      ns(a) * ns(b)
    else if sum < goal then
      pair(a + 1, b)
    else
      pair(a, b - 1)

  override def solutionA = pair(0, ns.size - 1)

  def triple(a: Int, b: Int): Int =
    if b == ns.size - 1 then
      triple(a + 1, a + 2)
    else if ns(a) + ns(b) > goal then
      triple(a, b + 1)
    else
      val sum = ns(a) + ns(b)
      ns.view.slice(b + 1, ns.size).map(_ + sum).search(goal) match
        case Found(i) => Seq(a, b, i + b + 1).map(ns).product
        case _ => triple(a, b + 1)

  override def solutionB = triple(0, 1)
