import scala.annotation.tailrec
import scala.collection.mutable

@main def day01() =
  val instructions = input(1).next
    .split(", ?")
    .map(i => (i.head, i.tail.toInt))

  def part1 = instructions.foldLeft(Point.empty) { case (pos, (direction, distance)) =>
    pos.turn(direction, distance)
  }.total

  def part2 =
    val visited = mutable.Set(Point.empty)

    @tailrec
    def aux(instructions: Array[(Char, Int)], pos: Point, bearing: Point): Int =
      val (direction, distance) = instructions.head
      val b = bearing.turn(direction)
      var next = Point.empty
      val found = (1 to distance).exists { d =>
        next = Point(pos.x + (d * b.x), pos.y + (d * b.y))
        !visited.add(next)
      }
      if (found) next.total
      else aux(instructions.tail, next, b)

    aux(instructions, Point.empty, Point.north)

  println(part1) // 307
  println(part2) // 165

case class Point(x: Int, y: Int):
  def turn(direction: Char, distance: Int = 0) =
    if (direction == 'L')
      Point(-y + distance, x) // equivalent to multiplying by i, the imaginary unit
    else
      Point(y + distance, -x) // equivalent to multiplying by -i

  def total = math.abs(x) + math.abs(y)

object Point:
  val empty = Point(0, 0)
  val north = Point(0, 1)
