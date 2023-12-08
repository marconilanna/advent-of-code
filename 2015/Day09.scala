@main def day09() =
  val extractor = """(\w+) to (\w+) = (\d+)""".r

  val distances = input(9).map { case extractor(from, to, dist) =>
    Set(from, to) -> dist.toInt
  }.toMap

  val routes = distances
    .keySet
    .flatten
    .toSeq
    .permutations
    .map {
      _.sliding(2)
        .map(r => distances(r.toSet))
        .sum
    }.toSeq

  def part1 = routes.min

  def part2 = routes.max

  println(part1) // 141
  println(part2) // 736
