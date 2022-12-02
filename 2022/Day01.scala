@main def day01() =
  def part1 = math.max.tupled {
    input(1).foldLeft(0, 0) { case ((max, acc), line) =>
      if (line.isEmpty) (math.max(max, acc), 0)
      else (max, acc + line.toInt)
    }
  }

  def part2 =
    val t = input(1).foldLeft(Top3.empty, 0) { case ((top3, acc), line) =>
      if (line.isEmpty) (top3 << acc, 0)
      else (top3, acc + line.toInt)
    }
    (t._1 << t._2).total

  println(part1) // 68442
  println(part2) // 204837

case class Top3(first: Int, second: Int, third: Int):
  def <<(i: Int) =
    if (i > first) Top3(i, first, second)
    else if (i > second) Top3(first, i, second)
    else if (i > third) Top3(first, second, i)
    else this

  def total = first + second + third

object Top3:
  val empty = Top3(0, 0, 0)
