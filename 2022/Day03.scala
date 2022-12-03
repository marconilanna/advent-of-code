import scala.collection.BitSet

@main def day03() =
  def priority(c: Char) =
    if (c < 'a') c - 'A' + 27
    else c - 'a' + 1

  def intersect(input: String*): Int = input
    .map(_.view.map(priority).to(BitSet))
    .reduce(_.intersect(_))
    .head

  def part1 = input(3).map { line =>
    val (a, b) = line.splitAt(line.length / 2)
    intersect(a, b)
  }.sum

  def part2 = input(3).grouped(3).map(intersect).sum

  println(part1) // 7793
  println(part2) // 2499
