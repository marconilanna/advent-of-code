@main def day08() =
  def part1 = input(8).map {
    _.sliding(2).foldLeft(0, false) { case ((acc, escaped), s) =>
      if (!escaped && s.head == '\\')
        val c = if (s.last == 'x') 3 else 1
        (acc + c, s.last == '\\')
      else (acc, false)
    }._1 + 2
  }.sum

  def part2 = input(8).map(_.count(c => c == '"' || c == '\\') + 2).sum

  println(part1) // 1371
  println(part2) // 2117
