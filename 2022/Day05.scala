import scala.collection.mutable.ArrayDeque

@main def day05() =
  def parse(op: (Int, ArrayDeque[Char], ArrayDeque[Char]) => Unit) =
    val size =
      val s = input(5).find(_(1) == '1').get
      s.drop(s.lastIndexOf(' ') + 1).toInt
    val stacks = IndexedSeq.fill(size)(ArrayDeque.empty[Char])

    def stack(line: String) =
      for ((p, i) <- 1.until(line.size).by(4).zipWithIndex)
        val c = line(p)
        if (c != ' ') stacks(i).append(c)

    def move(line: String) =
      val t = line.split(' ')
      val (n, from, to) = (t(1).toInt, t(3).toInt - 1, t(5).toInt - 1)
      op(n, stacks(from), stacks(to))

    input(5).foldLeft(stack) { case (f, line) =>
      if (line.isEmpty || line(1) == '1') move
      else
        f(line)
        f
    }
    stacks.map(_.head).mkString

  def part1 = parse { case (n, from, to) =>
    for (_ <- 1 to n)
      to.prepend(from.removeHead())
  }

  def part2 = parse { case (n, from, to) =>
    to.prependAll(from.take(n))
    from.remove(0, n)
  }

  println(part1) // FCVRLMVQP
  println(part2) // RWLWGJGFD
