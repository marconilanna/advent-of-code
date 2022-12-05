@main def day01() =
  val digits: IndexedSeq[Int] = input(1).next.map(_ - '0')

  def part1 = digits.foldLeft(digits.last, 0) { case ((prev, acc), curr) =>
    val inc = if (curr == prev) curr else 0
    (curr, acc + inc)
  }._2

  def part2 =
    val steps = digits.size / 2
    digits.zipWithIndex.foldLeft(0) { case (acc, (curr, i)) =>
      val pair = (i + steps) % digits.size
      val inc = if (curr == digits(pair)) curr else 0
      acc + inc
    }

  println(part1) // 1253
  println(part2) // 1278
