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
  val directions = range.flatMap(x => range.map(y => Point(x, y)).filter(_ != Point(0, 0)))

  def fixedPoint(f: (Point => Option[Boolean]) => (Boolean, Point) => Boolean): Int =
    def iterate(state: IndexedSeq[IndexedSeq[Option[Boolean]]]): Int =
      val next = state.zipWithIndex.map((row, y) => row.zipWithIndex.map(
        (e, x) => e.map(v => f(p => state(p.y)(p.x))(v, Point(x, y))))
      )
      if next == state then state.flatten.count(_.exists(identity)) else iterate(next)
    iterate(map)

  override def solutionA = fixedPoint { f => (e, p) =>
    val occupied = directions.map(p + _).filter(inBounds).map(f).count(_.exists(identity))
    if e then occupied < 4 else occupied == 0
  }

  def scan(f: Point => Option[Boolean])(p: Point, direction: Point): Boolean =
    val next = p + direction
    inBounds(next) && f(next).getOrElse(scan(f)(next, direction))

  override def solutionB = fixedPoint { f => (e, p) =>
    val occupied = directions.count(d => scan(f)(p, d))
    if e then occupied < 5 else occupied == 0
  }
