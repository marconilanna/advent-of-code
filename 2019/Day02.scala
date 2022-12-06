@main def day02() =
  def load = input(2).next.split(',').map(_.toInt)

  def eval(mem: Array[Int]) = mem.grouped(4).exists { case Array(op, a, b, c) =>
    op match
      case 1  => mem(c) = mem(a) + mem(b); false
      case 2  => mem(c) = mem(a) * mem(b); false
      case 99 => true
  }

  def part1 =
    val mem = load
    mem(1) = 12
    mem(2) = 2
    eval(mem)
    mem(0)

  def part2 =
    val target = 19690720
    (0 to 9999).find { n =>
      val mem = load
      mem(1) = n / 100
      mem(2) = n % 100
      eval(mem)
      mem(0) == target
    }.get

  println(part1) // 4945026
  println(part2) // 5296
