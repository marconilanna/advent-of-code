import scala.collection.mutable

@main def day06() =
  def grid[A](zero: A)(on: A => A, off: A => A, toggle: A => A)(f: (Int, mutable.IndexedSeq[A]) => Int) =
    def splitRange(s: String) =
      val Array(a, b) = s.split(',')
      (a.toInt, b.toInt)

    val size = 1000
    val lights = IndexedSeq.fill(size)(mutable.IndexedSeq.fill(size)(zero))
    input(6).map { line =>
      val tokens = line.split(' ')
      val t = if (tokens.head == "turn") tokens.tail else tokens
      (t(0), splitRange(t(1)), splitRange(t(3)))
    }.foreach { case (command, (a, b), (c, d)) =>
      val cmd = command match
        case "on"     => on
        case "off"    => off
        case "toggle" => toggle
      for
        i <- a to c
        j <- b to d
      do lights(i)(j) = cmd(lights(i)(j))
    }
    lights.foldLeft(0)(f)

  def part1 = grid(false)(_ => true, _ => false, !_)(_ + _.count(identity))

  def part2 = grid(0)(1.+, i => 0.max(i - 1), 2.+)(_ + _.sum)

  println(part1) // 400410
  println(part2) // 15343601
