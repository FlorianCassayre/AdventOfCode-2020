package adventofcode.solutions

import adventofcode.Day

object Day08 extends Day(8):

  val regex = "([a-z]+) ([+-][0-9]+)".r

  enum Instruction:
    case Acc(value: Int) extends Instruction
    case Jmp(value: Int) extends Instruction
    case Nop(value: Int) extends Instruction

  import Instruction._

  val instructions = lines.map {
    case regex(name, valueStr) =>
      val value = valueStr.toInt
      name match
        case "acc" => Acc(value)
        case "jmp" => Jmp(value)
        case "nop" => Nop(value)
  }

  def execute(pc: Int = 0, acc: Int = 0, history: Set[Int] = Set.empty)
             (implicit instructions: IndexedSeq[Instruction]): (Int, Boolean) =
    if pc == instructions.size then
      (acc, true)
    else if history.contains(pc) then
      (acc, false)
    else
      val present = history + pc
      instructions(pc) match
        case Acc(value) => execute(pc + 1, acc + value, present)
        case Jmp(value) => execute(pc + value, acc, present)
        case Nop(value) => execute(pc + 1, acc, present)

  override def solutionA = execute()(instructions)._1

  override def solutionB = instructions.zipWithIndex.flatMap { (instruction, i) =>
    val opt = instruction match
      case Acc(value) => None
      case Jmp(value) => Some(Nop(value))
      case Nop(value) => Some(Jmp(value))
    opt.flatMap(replacement =>
      execute()(instructions.updated(i, replacement)) match
        case (acc, true) => Some(acc)
        case _ => None
    )
  }.head
