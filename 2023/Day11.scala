import scala.annotation.tailrec

@main def day11() =
  val image = input(11).toIndexedSeq

  val rows = image.map(!_.contains('#'))
  val cols = image.transpose.map(!_.contains('#'))

  val galaxies = image.zipWithIndex.flatMap { (line, i) =>
    line.zipWithIndex.collect {
      case ('#', j) => (i, j)
    }
  }

  @tailrec
  def distances(galaxies: Seq[(Int, Int)], age: Long, acc: Seq[Long] = Seq.empty): Seq[Long] =
    def expand(space: Seq[Boolean], a: Int, b: Int) =
      (a - b).abs + space.slice(a.min(b), a.max(b)).count(identity) * age

    if (galaxies.isEmpty) acc
    else
      val (i, j) = galaxies.head
      val d = galaxies.tail.map(expand(rows, i, _) + expand(cols, j, _))
      distances(galaxies.tail, age, d ++ acc)

  def part1 = distances(galaxies, 1L).sum

  def part2 = distances(galaxies, 999999L).sum

  println(part1) // 9536038
  println(part2) // 447744640566
