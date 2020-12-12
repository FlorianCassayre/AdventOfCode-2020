package adventofcode

import java.io.{File, PrintWriter}

import scala.io.Source
import scala.util.{Failure, Success, Try}

abstract class Day(day: Int) extends App:

  require((1 to 25).contains(day), "Invalid day number")

  protected val lineSeparator: String = "\n"

  private val formatted = "%02d".format(day)

  private def readInput(): String = Source.fromFile(s"input/$formatted.txt").getLines().mkString(lineSeparator)

  private def writeOutput(output: String, part: Char): Unit =
    val path = s"output/$formatted$part.txt"
    new File(path).getParentFile.mkdirs()
    new PrintWriter(path):
      write(output)
      close()

  protected val input: String = readInput()
  protected lazy val lines: IndexedSeq[String] = input.split(lineSeparator).toIndexedSeq

  type Solution = String | Int | Long | BigInt

  def solutionA: Solution
  def solutionB: Solution

  private final def submit(): Unit =
    submitSolution(solutionA, 'A')
    submitSolution(solutionB, 'B')

  private def submitSolution(function: => Solution, part: Char): Unit =
    Try(function.toString) match
      case Success(string) =>
        writeOutput(string, part)
        println(string)
        println()
      case Failure(_: NotImplementedError) =>
        println(s"(ignored solution $part)")
        println()
      case Failure(exception) =>
        throw new RuntimeException(exception)

  private val mainThread: Thread = Thread.currentThread()
  new Thread(() => {
    mainThread.join()
    submit()
  }).start()
