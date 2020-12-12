ThisBuild / scalaVersion := "3.0.0-M2"
ThisBuild / organization := "me.cassayre.florian"

lazy val root = project
  .in(file("."))
  .settings(
    name := "AdventOfCode-2020",
    description := "Advent of Code 2020",
    version := "0.1.0"
  )

commands += Command("day") { _ =>
  import complete.DefaultParsers._
  (' ' ~ charClass(_.isDigit, "digit").+.map(_.mkString.toInt)).map(_._2)
} { case (previousState, i: Int) =>
  val formatted = "%02d".format(i)
  Command.process(s"runMain adventofcode.solutions.Day$formatted", previousState)
}
