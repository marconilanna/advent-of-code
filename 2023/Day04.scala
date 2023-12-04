@main def day04() =
  def cards = input(4).map { line =>
    val Array(winning, numbers) = line
      .split(':')
      .last
      .split('|')
      .map {
        _.split(' ')
          .filter(_.nonEmpty)
      }
    numbers.count(winning.toSet)
  }

  def part1 = cards.map { score =>
    if (score < 3) score else math.pow(2, score - 1).toInt
  }.sum

  def part2 =
    val queue = collection.mutable.Queue(1)
    cards.foldLeft(0) { (sum, score) =>
      val count = if (queue.isEmpty) 1 else queue.dequeue()
      for (i <- 0 until score) {
        if (i < queue.size) queue.update(i, queue(i) + count)
        else queue.append(count + 1)
      }
      sum + count
    }

  println(part1) // 21088
  println(part2) // 6874754
