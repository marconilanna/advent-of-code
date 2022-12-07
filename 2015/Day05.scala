@main def day05() =
  def part1 =
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val blacklist = Seq("ab", "cd", "pq", "xy")
    input(5).count { line =>
      line.count(vowels) >= 3 &&
      line.sliding(2).exists(s => s.head == s.last) &&
      !blacklist.exists(line.contains)
    }

  def part2 =
    val pair = """(..).*\1""".r.unanchored
    input(5).count { line =>
      pair.matches(line) &&
      line.sliding(3).exists(s => s.head == s.last)
    }

  println(part1) // 255
  println(part2) // 55
