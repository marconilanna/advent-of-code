@main def day06() =
  def marker(size: Int) = size + input(6).next.sliding(size).indexWhere(_.distinct.size == size)

  def part1 = marker(4)

  def part2 = marker(14)

  println(part1) // 1658
  println(part2) // 2260
