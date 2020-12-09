package adventofcode

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.{Failure, Success, Try}

abstract class Day(day: Int) extends App {

  private sealed abstract class SubProblem(val name: String)
  private case object A extends SubProblem("A")
  private case object B extends SubProblem("B")

  require(day >= 1 && day <= 25, "Invalid day number")

  protected val lineSeparator: String = "\n"

  private val formatted = "%02d".format(day)

  private def readInput(): String = Source.fromFile(s"input/$formatted.txt").getLines().mkString(lineSeparator)

  private def writeOutput(output: String, subProblem: SubProblem): Unit = {
    val sub = subProblem.name
    val path = s"output/$formatted$sub.txt"
    new File(path).getParentFile.mkdirs()
    new PrintWriter(path) {
      write(output)
      close()
    }
  }

  protected val input: String = readInput()
  protected val lines: IndexedSeq[String] = input.split(lineSeparator).toIndexedSeq

  type Solution = String | Int | BigInt

  def solutionA: Solution
  def solutionB: Solution


  // Submission

  private final def submit(): Unit = {
    submitSolution(solutionA, A)
    submitSolution(solutionB, B)
  }

  private def submitSolution(function: => Solution, subProblem: SubProblem): Unit = {
    Try(function.toString) match {
      case Success(string) =>
        writeOutput(string, subProblem)
        println(string)
        println()
      case Failure(_: NotImplementedError) =>
        println(s"(ignored solution ${subProblem.name})")
        println()
      case Failure(exception) =>
        throw new RuntimeException(exception)
    }
  }

  private val mainThread: Thread = Thread.currentThread()
  new Thread(() => {
    mainThread.join()
    submit()
  }).start()

}