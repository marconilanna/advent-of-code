@main def day02() =
  def part1 =
    val (a, b) = input(2).foldLeft(0, 0) { case ((a, b), line) =>
      val counts = line.groupBy(identity).map(_._2.size)
      (a + counts.find(2.==).size, b + counts.find(3.==).size)
    }
    a * b

  def part2 =
    var answer = ""
    input(2).exists { a =>
      input(2).exists { b =>
        if (a.zip(b).count(_ != _) == 1)
          answer = a.intersect(b)
          true
        else false
      }
    }
    answer

  println(part1) // 6200
  println(part2) // xpysnnkqrbuhefmcajodplyzw
