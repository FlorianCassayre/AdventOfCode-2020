package adventofcode.solutions

import adventofcode.Day

import scala.collection.{View, immutable}
import scala.util.chaining._
import scala.language.implicitConversions

object Day20 extends Day(20):

  case class Tile(id: Int, value: IndexedSeq[IndexedSeq[Boolean]]):
    def rotate(k: Int): Tile = if k > 0 then copy(value = value.head.indices.map(j => value.indices.map(i => value(value.size - i - 1)(j)))).rotate(k - 1) else this
    def flip: Tile = copy(value = value.transpose)
    def variants: Seq[Tile] = (0 until 4).map(rotate).flatMap(tile => Seq(tile, tile.flip))

  def parse(c: Char, space: Char): Boolean = c match
    case '#' => true
    case `space` => false

  val tiles = input.split(lineSeparator * 2).toVector.map(_.split(lineSeparator).toVector).map { s =>
    val tileId = s.head match
      case s"Tile $id:" => id.toInt
    val value = s.tail.toVector.map(_.map(parse(_, '.')).toVector)
    Tile(tileId, value)
  }
  val size = Math.sqrt(tiles.size).toInt

  val sizeRange = 0 until size
  val initialIndices = for i <- sizeRange; j <- sizeRange yield (i, j)

  val neighbouring = Seq((0, -1), (1, 0), (0, 1), (-1, 0))
  val inverseNeigbour = Seq(1, 0, 3, 2)

  val allVariants = tiles.flatMap(_.variants)
  val adjacency = allVariants.map { variant =>
    variant -> (0 until 4).map { i =>
      val head = variant.rotate((4 - i) % 4).value.head.reverse
      val results = allVariants.filter(_.id != variant.id).filter(_.rotate((4 - i + 2) % 4).value.head == head)
      assert(results.sizeIs <= 1)
      results.headOption
    }
  }.toMap

  override def solutionA = adjacency.filter(_._2.count(_.isEmpty) == 2).map(_._1.id.toLong).toSeq.distinct.product

  val corners = adjacency.filter(_._2.count(_.isEmpty) == 2)

  def solve(grid: IndexedSeq[IndexedSeq[Option[Tile]]], remainingIndices: Seq[(Int, Int)]): IndexedSeq[IndexedSeq[Tile]] =
    remainingIndices match
      case (i, j) +: leftIndices =>
        val chosen =
          if i == 0 && j == 0 then
            corners.filter(t => t._2(0).isEmpty && t._2(3).isEmpty).head._1
          else
            neighbouring.map { case (i1, j1) => (i + i1, j + j1) }.zip(inverseNeigbour).filter { case ((i1, j1), _) => sizeRange.contains(i1) && sizeRange.contains(j1) }
              .collect { case ((i1, j1), inverse) if grid(i1)(j1).nonEmpty => adjacency(grid(i1)(j1).get)(inverse) }
              .flatten.head
        solve(grid.updated(i, grid(i).updated(j, Some(chosen))), leftIndices)
      case _ => grid.map(_.map(_.get))

  val solved = solve(IndexedSeq.fill(size, size)(None), initialIndices)

  val solutions = Tile(0, solved.map(_.map(_.value.tail.init.map(_.tail.init))).flatMap(row => row.head.indices.map(i => row.flatMap(_(i))))).variants.map(_.value)

  val pattern = """                  # 
                  |#    ##    ##    ###
                  | #  #  #  #  #  #   """.stripMargin.split(lineSeparator).map(_.map(parse(_, ' ')))

  val monsters = solutions.flatMap(stripped => stripped.zipWithIndex.dropRight(pattern.size - 1).flatMap { case (row, i) => row.indices.dropRight(pattern.head.size - 1).flatMap {
    j =>
      if pattern.zipWithIndex.forall { case (row1, i1) => row1.zipWithIndex.forall {
        case (valuePattern, j1) => !valuePattern || stripped(i + i1)(j + j1)
      }} then Some(pattern.zipWithIndex.flatMap { case (row1, i1) => row1.zipWithIndex.collect {
        case (valuePattern, j1) if valuePattern => (i + i1, j + j1)
      }}) else None
  }}).flatten.toSet.size

  override def solutionB = solutions.head.map(_.count(identity)).sum - monsters
