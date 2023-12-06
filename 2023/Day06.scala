@main def day06() =
  def races[A](f: String => A) =
    val r = input(6).map { line =>
      f(line.split(':').last)
    }
    (r.next, r.next)

  // -t^2 + Tt - d = 0
  val ways: ((Double, Double)) => Int = (time, distance) =>
    val delta = math.sqrt(time * time - 4 * distance)
    val x1 = (time - delta) / 2
    val x2 = (time + delta) / 2
    (x2.ceil - x1.floor - 1).toInt

  def part1 =
    val (times, distances) = races {
      _.split(' ')
        .filter(_.nonEmpty)
        .map(_.toDouble)
    }
    times
      .zip(distances)
      .map(ways)
      .product

  def part2 = ways {
    races {
      _.filter(_.isDigit).toDouble
    }
  }

  println(part1) // 1312850
  println(part2) // 36749103
