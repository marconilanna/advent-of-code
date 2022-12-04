@main def day04() =
  def parse(line: String) = line.split("[-,]").map(_.toInt)

  def part1 = input(4).map(parse).count { case Array(a, b, c, d) =>
    (a <= c && b >= d) ||
    (a >= c && b <= d)
  }

  def part2 = input(4).map(parse).count { case Array(a, b, c, d) =>
    !(a > d) && !(c > b)
  }

  println(part1) // 456
  println(part2) // 808
