package adventofcode.solutions

import adventofcode.Day

object Day11 extends Day(11):

  case class Point(x: Int, y: Int) {
    def +(that: Point): Point = Point(x + that.x, y + that.y)
  }

  val map = lines.map(_.map {
    case 'L' => Some(false)
    case '.' => None
  })

  type Board = IndexedSeq[IndexedSeq[Option[Boolean]]]

  def inBounds(p: Point): Boolean = p.x >= 0 && p.x < map.head.size && p.y >= 0 && p.y < map.size

  val range = -1 to 1
  val directions = range.flatMap(x => range.map(y => Point(x, y)).filter(_ != Point(0, 0)))

  def pr(o: Option[Boolean]): Char = o match
    case Some(false) => 'L'
    case Some(true) => '#'
    case None => '.'

  def fixedPoint(f: Board => (Boolean, Point) => Boolean): Int =
    def iterate(state: Board): Int =
      val next: Board = state.zipWithIndex.map((row, y) => row.zipWithIndex.map((e, x) => e.map(v => f(state)(v, Point(x, y)))))
      if next == state then state.flatten.count(_.exists(identity)) else iterate(next)
    iterate(map)

  override def solutionA = fixedPoint { board => (e, p) =>
    val occupied = directions.map(p + _).filter(inBounds).count(p => board(p.y)(p.x).exists(identity))
    if e then (occupied < 4) else (occupied == 0)
  }

  def scan(f: Point => Option[Boolean])(p: Point, direction: Point): Boolean =
    val next = p + direction
    if inBounds(next) then
      f(next) match
        case Some(v) => v
        case None => scan(f)(next, direction)
    else
      false

  override def solutionB = fixedPoint { board => (e, p) =>
    val occupied = directions.count(d => scan(pt => board(pt.y)(pt.x))(p, d))
    if e then (occupied < 5) else (occupied == 0)
  }
