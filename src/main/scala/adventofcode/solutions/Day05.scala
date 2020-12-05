package adventofcode.solutions

import adventofcode.Day

object Day05 extends Day(5):

  case class Seat(r: Int, c: Int):
    val id = r * 8 + c

  val (rows, columns) = (0 until 128, 0 until 8)

  def toSeat(code: String): Seat =
    val (Seq(r), Seq(c)) = code.foldLeft(rows, columns) { case ((r, c), h) =>
      val (rh, ch) = (r.size / 2, c.size / 2)
      h match
        case 'F' => (r.dropRight(rh), c)
        case 'B' => (r.drop(rh), c)
        case 'L' => (r, c.dropRight(ch))
        case 'R' => (r, c.drop(ch))
    }
    Seat(r, c)

  val seats = lines.map(toSeat)

  override def solutionA = seats.map(_.id).max

  val seatsIds = seats.map(_.id).toSet

  val found =
    for
    r <- rows.drop(1).dropRight(1)
    c <- columns
    seat = Seat(r, c)
    if !seatsIds.contains(seat.id)
    if seatsIds.contains(seat.id - 1) && seatsIds.contains(seat.id + 1)
    yield seat

  override def solutionB = found.head.id
