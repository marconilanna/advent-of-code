import scala.collection.mutable

@main def day03() =
  val start = (0, 0)

  def move(x: Int, y: Int, direction: Char) = direction match
    case '^' => (x, y + 1)
    case 'v' => (x, y - 1)
    case '>' => (x + 1, y)
    case '<' => (x - 1, y)

  def part1 =
    val houses = mutable.Set(start)
    input(3).next.foldLeft(start) { case ((x, y), direction) =>
      val next = move(x, y, direction)
      houses.add(next)
      next
    }
    houses.size

  def part2 =
    val houses = mutable.Set(start)
    input(3).next.grouped(2).foldLeft(start, start) { case (((x, y), (w, z)), directions) =>
      val next1 = move(x, y, directions.head)
      val next2 = move(w, z, directions.last)
      houses.add(next1)
      houses.add(next2)
      (next1, next2)
    }
    houses.size

  println(part1) // 2565
  println(part2) // 2639
