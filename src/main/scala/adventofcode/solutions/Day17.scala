package adventofcode.solutions

import adventofcode.Day

object Day17 extends Day(17):

  case class Cube(x: Int, y: Int, z: Int = 0, w: Int = 0):
    def +(that: Cube): Cube = Cube(x + that.x, y + that.y, z + that.z, w + that.w)

  val space = lines.zipWithIndex.flatMap((rows, y) => rows.zipWithIndex.flatMap {
    case ('#', x) => Some(Cube(x, y))
    case ('.', _) => None
  }).toSet

  def simulate(fourDimensions: Boolean): Int =
    val r = -1 to 1
    val neighbours = for x <- r; y <- r; z <- r; w <- if fourDimensions then r else Seq(0); if Seq(x, y, z, w).exists(_ != 0) yield Cube(x, y, z, w)
    val states = LazyList.unfold(space) { s =>
      val next = s.flatMap(cube => neighbours.map(cube + _)).map { cube =>
        val active = neighbours.count(n => s.contains(cube + n))
        if s.contains(cube) then cube -> (2 to 3).contains(active) else cube -> (active == 3)
      }.collect { case (cube, true) => cube }
      Some(next.size, next)
    }
    states(6 - 1)

  override def solutionA = simulate(false)

  override def solutionB = simulate(true)
