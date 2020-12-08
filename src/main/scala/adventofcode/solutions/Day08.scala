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

  case class State(instructions: IndexedSeq[Instruction], pc: Int = 0, acc: Int = 0, history: Set[Int] = Set.empty)

  def execute(state: State): Either[Int, Int] =
    val State(instructions, pc, acc, history) = state
    if pc == instructions.size then
      Left(acc)
    else if history.contains(pc) then
      Right(acc)
    else
      val present = history + pc
      instructions(pc) match
        case Acc(value) => execute(state.copy(pc = pc + 1, acc = acc + value, history = present))
        case Jmp(value) => execute(state.copy(pc = pc + value, history = present))
        case Nop(value) => execute(state.copy(pc = pc + 1, history = present))

  override def solutionA = execute(State(instructions)).getOrElse(0)

  override def solutionB = instructions.zipWithIndex.flatMap { (instruction, i) =>
    val opt = instruction match
      case Acc(value) => None
      case Jmp(value) => Some(Nop(value))
      case Nop(value) => Some(Jmp(value))
    opt.flatMap { replacement =>
      val fixedInstructions = instructions.updated(i, replacement)
      execute(State(fixedInstructions)) match
        case Left(acc) => Some(acc)
        case Right(_) => None
    }
  }.head
