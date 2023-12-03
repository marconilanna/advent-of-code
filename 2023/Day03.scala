@main def day03() =
  val schematic = input(3).toIndexedSeq
  val width = schematic.head.length
  val height = schematic.length

  val extractor = """\d+""".r.findAllIn _

  def adjacent[A](i: Int, j: Int, s: String, symbol: Char => Boolean) =
    val l = s.length

    def bounded(j: Int) = if (j < 0) 0 else if (j > width) width else j

    def row(i: Int) = if (i < 0 || i == height) None else Option(
      schematic(i)
        .slice(bounded(j - 1), bounded(j + l + 1))
        .indexWhere(symbol)
    )
    .filter(_ > -1)
    .map(p => i -> (p + bounded(j - 1)))

    def col(j: Int) = if (j < 0 || j == width || !symbol(schematic(i)(j))) None else Option(i -> j)

    row(i - 1)
    .orElse(row(i + 1))
    .orElse(col(j - 1))
    .orElse(col(j + l))
    .map(_ -> s.toInt)

  def parts(symbol: Char => Boolean) = schematic
    .map(extractor)
    .zipWithIndex
    .flatMap { (m, i) =>
      m.flatMap(adjacent(i, m.start, _, symbol))
    }

  def part1 = parts(c => !(c == '.' || c.isDigit))
    .map(_._2)
    .sum

  def part2 = parts('*'.==)
    .groupBy(_._1)
    .values
    .filter(_.size == 2)
    .map(_.map(_._2).product)
    .sum

  println(part1) // 498559
  println(part2) // 72246648
