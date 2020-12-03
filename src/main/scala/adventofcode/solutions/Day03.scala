package adventofcode.solutions

import adventofcode.Day

import scala.util.chaining._
import scala.language.implicitConversions

object Day03 extends Day(3):

  val (width, height) = (lines.head.size, lines.size)

  def traverse(a: Int, b: Int) = (0 until height / b).count(i => lines(i * b)((a * i) % width) == '#')

  val firstSlope = (3, 1)

  override def solutionA = firstSlope.pipe(traverse(_, _))

  val allSlopes = Seq((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))

  override def solutionB = allSlopes.map(traverse(_, _)).product
