package adventofcode.solutions

import adventofcode.Day

object Day05 extends Day(5):

  case class Acc(c1: Int, c2: Int, r1: Int, r2: Int)
  case class Seat(c: Int, r: Int):
    val id = c * 8 + r
  
  val (rows, columns) = (128, 8)
  
  def toSeat(s: String): Seat =
    val result = s.foldLeft(Acc(0, rows, 0, columns)) { case (acc@Acc(c1, c2, r1, r2), c) =>
      val (cr, rr) = ((c2 - c1) / 2, (r2 - r1) / 2)
      c match
        case 'F' => acc.copy(c2 = c2 - cr)
        case 'B' => acc.copy(c1 = c1 + cr)
        case 'L' => acc.copy(r2 = r2 - rr)
        case 'R' => acc.copy(r1 = r1 + rr)
    }
    Seat(result.c1, result.r1)

  val seats = lines.map(toSeat)

  override def solutionA = seats.map(_.id).max

  val seatsIds = seats.map(_.id)

  val empty =
    for
    r <- (1 until (rows - 1))
    c <- (0 until columns)
    seat = Seat(r, c)
    if !seatsIds.contains(seat.id)
    if seatsIds.contains(seat.id - 1) && seatsIds.contains(seat.id + 1) 
    yield seat
  
  override def solutionB = empty.head.id
