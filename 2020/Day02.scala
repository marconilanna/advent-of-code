@main def day02() =
  val parser = """(\d+)-(\d+) (.): (.+)""".r

  def part1 = input(2).count { line =>
    val parser(min, max, l, password) = line: @unchecked
    val c = l.head
    val count = password.count(_ == c)
    count >= min.toInt && count <= max.toInt
  }

  def part2 = input(2).count { line =>
    val parser(a, b, l, password) = line: @unchecked
    val c = l.head
    (password(a.toInt - 1) == c) != (password(b.toInt - 1) == c)
  }

  println(part1) // 655
  println(part2) // 673
