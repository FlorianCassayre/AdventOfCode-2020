package adventofcode.solutions

import adventofcode.Day

object Day11 extends Day(11):

  case class Point(x: Int, y: Int):
    def +(that: Point): Point = Point(x + that.x, y + that.y)

  val map = lines.map(_.map {
    case 'L' => Some(false)
    case '.' => None
  })

  def inBounds(p: Point): Boolean = p.x >= 0 && p.x < map.head.size && p.y >= 0 && p.y < map.size

  val range = -1 to 1
  val directions = for y <- range; x <- range; if x != 0 || y != 0; yield Point(x, y)

  def fixedPoint(adjacency: (Point => Option[Boolean]) => (Point, Point) => Boolean, turnover: Int): Int =
    def iterate(state: IndexedSeq[IndexedSeq[Option[Boolean]]]): Int =
      val next = state.zipWithIndex.map((row, y) => row.zipWithIndex.map { (e, x) =>
        val p = Point(x, y)
        val occupied = directions.count(d => adjacency(q => state(q.y)(q.x))(p + d, d))
        e.map(if _ then occupied < turnover else occupied == 0)
      })
      if next == state then state.flatten.count(_.exists(identity)) else iterate(next)
    iterate(map)

  override def solutionA = fixedPoint(f => (p, _) => inBounds(p) && f(p).exists(identity), 4)

  def scan(f: Point => Option[Boolean])(p: Point, direction: Point): Boolean =
    inBounds(p) && f(p).getOrElse(scan(f)(p + direction, direction))

  override def solutionB = fixedPoint(scan, 5)
