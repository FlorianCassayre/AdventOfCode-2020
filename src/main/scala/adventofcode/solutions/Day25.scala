package adventofcode.solutions

import adventofcode.Day

object Day25 extends Day(25):

  val Seq(publicKey1, publicKey2) = lines.map(_.toInt).toSeq

  def stream(subject: Long): LazyList[Long] = LazyList.iterate(1L)(_ * subject % 20201227)

  override def solutionA = stream(7).zip(stream(publicKey2)).find((k, _) => k == publicKey1).get._2

  override def solutionB = ""
