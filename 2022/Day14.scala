import scala.annotation.tailrec
import scala.collection.mutable

@main def day14() =
  def parse(line: String) = line.split(" -> ").map { path =>
    val points = path.split(',').map(_.toInt)
    (points(0), points(1))
  }

  def scan =
    val cave = IndexedSeq.fill(1000)(mutable.BitSet.empty)
    input(14).map(parse).foreach {
      _.reduce { case ((a, b), (x, y)) =>
        for
          i <- a to x by (if (a > x) -1 else 1)
          j <- b to y by (if (b > y) -1 else 1)
        do cave(i) += j
        (x, y)
      }
    }
    cave

  def pour(cave: IndexedSeq[mutable.BitSet], f: Int => Unit = _ => ()) =
    val floor = cave.view.flatMap(_.maxOption).max + 2
    val path = mutable.Stack((500, 0))

    def drop(j: Int)(i: Int) =
      val fall = !cave(i)(j)
      if (fall) path.push((i, j))
      fall

    @tailrec
    def aux(count: Int): Int =
      if (path.isEmpty) count
      else
        val (i, j) = path.head
        val stepDown = drop(j + 1)
        if (j == floor) count
        else if (stepDown(i)) aux(count)
        else if (stepDown(i - 1)) aux(count)
        else if (stepDown(i + 1)) aux(count)
        else
          cave(i) += j
          path.pop
          aux(count + 1)

    f(floor)
    aux(0)

  def part1 = pour(scan)

  def part2 =
    val cave = scan
    pour(cave, floor => cave.foreach(_ += floor))

  println(part1) // 763
  println(part2) // 23921
