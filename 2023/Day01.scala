@main def day01() =
  def part1 = for {
    line <- input(1)
    first <- line.find(_.isDigit)
    last <- line.findLast(_.isDigit)
  } yield 10 * (first - '0') + (last - '0')

  def part2 =
    val digits = Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
      .zipWithIndex
      .toMap

    def digit(s: String) = if (s.head.isDigit) s.head - '0' else digits(s)

    def regex(s: Iterable[String]) = (s.mkString("|") + "|\\d").r
    val r = regex(digits.keys)
    val rr = regex(digits.keys.map(_.reverse))

    for {
      line <- input(1)
      first <- r.findFirstIn(line)
      last <- rr.findFirstIn(line.reverse)
    } yield 10 * digit(first) + digit(last.reverse)

  println(part1.sum) // 54927
  println(part2.sum) // 54581
