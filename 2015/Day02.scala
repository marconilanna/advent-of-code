@main def day02() =
  def dimensions = input(2).map(_.split('x').map(_.toInt))

  def part1 = dimensions.map { case Array(l, w, h) =>
    val areas = Seq(l * w, l * h, w * h)
    2 * areas.sum + areas.min
  }.sum

  def part2 = dimensions.map { d =>
    2 * (d.sum - d.max) + d.product
  }.sum

  println(part1) // 1588178
  println(part2) // 3783758
