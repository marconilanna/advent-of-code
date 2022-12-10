import scala.collection.mutable

@main def day09() =
  def parse(s: String) = (s.head, s.drop(2).toInt)

  def part1 =
    val visited = mutable.Set(Knot.empty)
    input(9).map(parse).foldLeft(Knot.empty, Knot.empty) { case ((head, tail), (direction, distance)) =>
      (1 to distance).foldLeft(head, tail) { case ((head, tail), _) =>
        val h = head.move(direction)
        val t = h.move(tail)
        visited += t
        (h, t)
      }
    }
    visited.size

  def part2 =
    val visited = mutable.Set(Knot.empty)
    input(9).map(parse).foldLeft(IndexedSeq.fill(10)(Knot.empty)) { case (rope, (direction, distance)) =>
      (1 to distance).foldLeft(rope) { case (acc, _) =>
        val head = acc.head.move(direction)
        val rope = acc.tail.foldLeft(IndexedSeq(head)) { case (knots, tail) =>
          knots :+ knots.last.move(tail)
        }
        visited += rope.last
        rope
      }
    }
    visited.size

  println(part1) // 6503
  println(part2) // 2724

case class Knot(i: Int, j: Int):
  val move = (_: Char) match
    case 'R' => copy(i = i + 1)
    case 'L' => copy(i = i - 1)
    case 'U' => copy(j = j + 1)
    case 'D' => copy(j = j - 1)

  def move(tail: Knot) =
    val (a, b) = (i - tail.i, j - tail.j)
    val (x, y) = (a - a.sign, b - b.sign)
    if (x.abs + y.abs > 0)
      Knot(i - x.sign, j - y.sign)
    else tail

object Knot:
  val empty = Knot(0, 0)
