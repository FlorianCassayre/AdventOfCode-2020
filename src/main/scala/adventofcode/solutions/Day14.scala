package adventofcode.solutions

import adventofcode.Day

object Day14 extends Day(14):

  val regexMask = "mask = ([01X]{36})".r
  val regexWrite = "mem\\[([0-9]+)\\] = ([0-9]+)".r

  enum Instruction:
    case Mask(zero: Long, one: Long, floating: Long)
    case Write(address: Long, value: Long)

  import Instruction._

  val instructions = lines.map {
    case regexMask(mask) =>
      def parse(c: Char): Long = mask.foldLeft(0L)((n, v) => n << 1 | (if v == c then 1 else 0))
      Mask(parse('0'), parse('1'), parse('X'))
    case regexWrite(address, value) =>
      Write(address.toLong, value.toLong)
  }

  def initialize(f: (Mask, Write) => Map[Long, Long]): Long = instructions.foldLeft((Option.empty[Mask], Map.empty[Long, Long])) {
    case ((mask, memory), instruction) =>
      instruction match
        case newMask: Mask => (Some(newMask), memory)
        case write: Write => (mask, memory ++ f(mask.get, write))
  }._2.values.sum

  override def solutionA = initialize { case (Mask(zero, one, floating), Write(address, value)) =>
    val newValue = (value & floating | one) & ~zero
    Map(address -> newValue)
  }

  override def solutionB = initialize { case (Mask(zero, one, floating), Write(address, value)) =>
    val floatingBits = (0 until 36).filter(i => ((floating >>> i) & 1) != 0)
    val power = (0 to floatingBits.size).flatMap(floatingBits.combinations)
    val addresses = power.map(_.foldLeft(address & zero | one)((acc, e) => (acc | (1L << e))))
    addresses.map(_ -> value).toMap
  }
