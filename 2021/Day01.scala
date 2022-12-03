@main def day01() =
  def part1 = input(1).foldLeft(Int.MaxValue, 0) { case ((prev, acc), line) =>
    val curr = line.toInt
    val increased = if (curr > prev) 1 else 0
    (curr, acc + increased)
  }._2

  def part2 = input(1).sliding(3).foldLeft(Int.MaxValue, 0) { case ((prev, acc), lines) =>
    val curr = lines.map(_.toInt).sum
    val increased = if (curr > prev) 1 else 0
    (curr, acc + increased)
  }._2

  println(part1) // 1791
  println(part2) // 1822
