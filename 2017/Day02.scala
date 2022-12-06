@main def day02() =
  def parse(line: String) = line.split('\t').map(_.toInt)

  def part1 = input(2).map(parse).map { values =>
    values.max - values.min
  }.sum

  def part2 = input(2).map(parse).map { values =>
    var b = 0
    val a = values.find { a =>
      values.exists { v =>
        b = v
        (a != v) && ((a % v == 0) || (v % a == 0))
      }
    }.get
    a.max(b) / a.min(b)
  }.sum

  println(part1) // 44887
  println(part2) // 242
