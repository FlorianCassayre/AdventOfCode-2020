package adventofcode.solutions

import adventofcode.Day

object Day20 extends Day(20):

  val tiles = input.split(lineSeparator + lineSeparator).toVector.map(_.split(lineSeparator).toVector).map { s =>
    val tileId = s.head match
      case s"Tile $id:" => id.toInt
    val value = s.tail.toVector.map(_.map(c => c == '#').toVector)
    tileId -> value
  }.toMap
  val (width, height) =
    val tile = tiles.values.head
    (tile.head.size, tile.size)

  val borders = tiles.view.mapValues(t => Seq(t.head, t.transpose.last, t.last.reverse, t.transpose.head.reverse)).toMap

  val adjacency: Map[Int, Map[Int, Seq[(Int, Int)]]] =
    borders.map((id, faces) =>
      id -> faces.zipWithIndex.map((face, faceIndex) =>
        faceIndex -> borders.filter((otherId, _) => otherId != id).toSeq.flatMap((otherId, otherOrientations) =>
          (otherOrientations ++ otherOrientations.map(_.reverse)).zipWithIndex.filter((otherOrientation, otherFaceId) =>
            otherOrientation.zip(face).forall(_ == _)
          ).map((_, i) => i).map(otherId -> _)
        )
      ).toMap
    ).toMap

  val corners = adjacency.filter((_, value) => value.values.count(_.nonEmpty) == 2).toSeq

  override def solutionA = corners.map((id, _) => id.toLong).product

  override def solutionB = ???
