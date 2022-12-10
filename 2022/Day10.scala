@main def day10() =
  def parse(l: String) = l.split(' ')
  def value(input: String) = if (input.head.isLetter) 0 else input.toInt

  def part1 = input(10).flatMap(parse).zip(LazyList.from(1)).foldLeft(0, 1) { case ((acc, register), (input, cycle)) =>
    val signal = if (cycle % 40 == 20) cycle * register else 0
    (acc + signal, register + value(input))
  }._1

  def part2 = input(10).flatMap(parse).zipWithIndex.foldLeft(Seq.empty[Char], 1) { case ((acc, register), (input, pos)) =>
    val pixel = if ((register - (pos % 40)).abs < 2) '#' else '.'
    (pixel +: acc, register + value(input))
  }._1.reverse.grouped(40).map(_.mkString)

  println(part1) // 17020
  part2.foreach(println) // RLEZFLGE
