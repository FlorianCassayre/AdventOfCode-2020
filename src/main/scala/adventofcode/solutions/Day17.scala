package adventofcode.solutions

import adventofcode.Day

object Day17 extends Day(17):

  val space = lines.zipWithIndex.flatMap((rows, y) => rows.zipWithIndex.flatMap {
    case ('#', x) => Some(Seq(x, y))
    case ('.', _) => None
  }).toSet

  def simulate(dimensions: Int): Int =
    val neighbours = Seq.fill(dimensions)(-1 to 1).flatten.combinations(dimensions).flatMap(_.permutations).filter(_.exists(_ != 0)).toSeq
    val states = LazyList.unfold(space.map(_.padTo(dimensions, 0))) { s =>
      val next = s.flatMap(cube => neighbours.map(_.zip(cube).map(_ + _))).map { cube =>
        val active = neighbours.map(_.zip(cube).map(_ + _)).count(s.contains)
        if s.contains(cube) then cube -> (2 to 3).contains(active) else cube -> (active == 3)
      }.collect { case (cube, true) => cube }
      Some(next.size, next)
    }
    states(6 - 1)

  override def solutionA = simulate(3)

  override def solutionB = simulate(4)
