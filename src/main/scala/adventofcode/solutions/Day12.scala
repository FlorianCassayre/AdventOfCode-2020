package adventofcode.solutions

import adventofcode.Day

object Day12 extends Day(12):

  case class Vec(x: Int, y: Int):
    def +(that: Vec): Vec = Vec(x + that.x, y + that.y)
    def *(v: Int): Vec = Vec(x * v, y * v)
    def rotate(v: Int): Vec = if v > 0 then Vec(-y, x).rotate(v - 1) else this
    def manhattan: Int = x.abs + y.abs

  val Zero = Vec(0, 0)

  enum Action:
    case Cardinal(direction: Vec)
    case Rotation(amount: Int)
    case Forward(steps: Int)

  import Action._

  val angle = 90

  val actions = lines.map(s => (s.head, s.tail.toInt)).map((s, v) => s match
    case 'N' => Cardinal(Vec(0, v))
    case 'S' => Cardinal(Vec(0, -v))
    case 'E' => Cardinal(Vec(v, 0))
    case 'W' => Cardinal(Vec(-v, 0))
    case 'L' => Rotation(v / angle)
    case 'R' => Rotation(4 - v / angle)
    case 'F' => Forward(v)
  )

  override def solutionA = actions.foldLeft((Zero, Vec(1, 0))) { case ((position, face), action) =>
    action match
      case Cardinal(direction) => (position + direction, face)
      case Rotation(amount) => (position, face.rotate(amount))
      case Forward(steps) => (position + face * steps, face)
  }._1.manhattan

  override def solutionB = actions.foldLeft((Zero, Vec(10, 1))) { case ((position, waypoint), action) =>
    action match
      case Cardinal(direction) => (position, waypoint + direction)
      case Rotation(amount) => (position, waypoint.rotate(amount))
      case Forward(steps) => (position + waypoint * steps, waypoint)
  }._1.manhattan
