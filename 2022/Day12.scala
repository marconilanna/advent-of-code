import scala.annotation.tailrec
import scala.collection.mutable

@main def day12() =
  val grid = input(12).toIndexedSeq
  val (m, n) = (grid.size, grid(0).size)

  def elev(i: Int, j: Int) = grid(i)(j) match
    case 'E' => 'z'
    case c   => c.max('a')

  def path(start: Char, target: Char, climb: (Int, Int) => Boolean): Int =
    val (j, i) = grid.view.map(_.indexWhere(start.==)).zipWithIndex.find(_._1 > -1).get
    val unvisited = mutable.PriorityQueue((i, j, 0))(Ordering.by(-_._3))
    val distance = IndexedSeq.fill(m)(mutable.IndexedSeq.fill(n)(Int.MaxValue))

    def neighbors(i: Int, j: Int, d: Int) = Seq(
      (i + 1, j, d),
      (i - 1, j, d),
      (i, j + 1, d),
      (i, j - 1, d)
    ).filter { case (x, y, _) =>
      x >= 0 && x < m &&
      y >= 0 && y < n &&
      climb(elev(i, j), elev(x, y)) &&
      distance(x)(y) > d
    }

    @tailrec
    def aux: Int =
      val (i, j, d) = unvisited.dequeue
      if (grid(i)(j) == target) d
      else
        if (distance(i)(j) > d)
          distance(i)(j) = d
          unvisited.enqueue(neighbors(i, j, d + 1): _*)
        aux

    aux

  def part1 = path('S', 'E', _ + 1 >= _)

  def part2 = path('E', 'a', _ - 1 <= _)

  println(part1) // 447
  println(part2) // 446
