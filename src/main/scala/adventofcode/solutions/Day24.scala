package adventofcode.solutions

import adventofcode.Day

object Day24 extends Day(24):

  case class Point(x: Int = 0, y: Int = 0, z: Int = 0):
    def +(that: Point): Point = Point(x + that.x, y + that.y, z + that.z)

  val adjacency = Map(
    "ne" -> Point(y = 1, z = -1),
    "e" -> Point(x = 1, z = -1),
    "se" -> Point(x = 1, y = -1),
    "sw" -> Point(y = -1, z = 1),
    "w" -> Point(x = -1, z = 1),
    "nw" -> Point(x = -1, y = 1)
  )
  val neighbouring = adjacency.values.toSet

  val regex = "((?:n|s)?(?:e|w))(.*)".r

  val directions = lines.map(s => Seq.unfold(s) {
    case regex(d, tail) => Some(adjacency(d), tail)
    case "" => None
  })

  extension [T](set: Set[T])
    infix def ^(e: T): Set[T] = if set.contains(e) then set - e else set + e

  val initial = directions.foldLeft(Set.empty[Point])((acc, e) => acc ^ e.reduce(_ + _))

  override def solutionA = initial.size

  val sequence = LazyList.iterate(initial)(tiles =>
    (tiles ++ tiles.flatMap(tile => neighbouring.map(tile + _))).flatMap { tile =>
      val neighbours = neighbouring.map(tile + _).count(tiles)
      val color = if tiles.contains(tile) then (1 to 2).contains(neighbours) else neighbours == 2
      if color then Some(tile) else None
    }
  )

  override def solutionB = sequence(100).size
