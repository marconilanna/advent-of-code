@main def day02() =
  def games = input(2).map { line =>
    val Array(id, game) = line.split(':')
    (
      id.split(' ').last.toInt,
      game.split(",|;").map {
        _.trim.split(' ')
      }
    )
  }

  val colors = Map("red" -> 12, "green" -> 13, "blue" -> 14)

  def part1 = games.collect {
    case (id, sets) if sets.forall { case Array(count, color) =>
      count.toInt <= colors(color)
    } => id
  }.sum

  def part2 = games.map { (_, sets) =>
    sets
      .groupBy(_.last)
      .values
      .map {
        _.map(_.head.toInt).max
      }.product
  }.sum

  println(part1) // 2545
  println(part2) // 78111
