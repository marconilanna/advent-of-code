@main def day02() =
  val (loss, draw, win) = (0, 3, 6)
  def parse(line: String) = (line(0) - 'A', line(2) - 'X')

  def part1 =
    val score = IndexedSeq(draw, win, loss)
    input(2).map { line =>
      val (opponent, me) = parse(line)
      val outcome = (me - opponent + 3) % 3
      score(outcome) + me + 1
    }.sum

  def part2 =
    val score = IndexedSeq(loss, draw, win)
    input(2).map { line =>
      val (opponent, outcome) = parse(line)
      val me = (opponent + outcome + 2) % 3
      score(outcome) + me + 1
    }.sum

  println(part1) // 15337
  println(part2) // 11696
