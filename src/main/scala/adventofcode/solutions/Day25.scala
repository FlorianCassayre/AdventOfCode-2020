package adventofcode.solutions

import adventofcode.Day

object Day25 extends Day(25):

  val Seq(publicKey1, publicKey2) = lines.map(_.toInt).toSeq
  
  def transform(value: Int, subject: Int): Int = ((value.toLong * subject) % 20201227).toInt

  def pow(v: Int, e: Int, acc: Int = 1): Int = if e > 0 then pow(v, e - 1, transform(acc, v)) else acc

  def crack(public: Int, subject: Int = 7, loop: Int = 0, acc: Int = 1): Int =
    if acc != public then
      crack(public, subject, loop + 1, transform(acc, subject))
    else
      loop

  val (loop1, loop2) = (crack(publicKey1), crack(publicKey2))

  override def solutionA = pow(publicKey2, loop1)

  override def solutionB = ""
