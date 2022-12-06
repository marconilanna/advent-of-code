@main def day02() =
  def part1 =
    val (pos, depth) = input(2).foldLeft(0, 0) { case ((pos, depth), command) =>
      command.split(' ') match
        case Array("forward", i) => (pos + i.toInt, depth)
        case Array("down", i)    => (pos, depth + i.toInt)
        case Array("up", i)      => (pos, depth - i.toInt)
    }
    pos * depth

  def part2 =
    val (pos, depth, _) = input(2).foldLeft(0, 0, 0) { case ((pos, depth, aim), command) =>
      command.split(' ') match
        case Array("forward", i) => val n = i.toInt; (pos + n, depth + n * aim, aim)
        case Array("down", i)    => (pos, depth, aim + i.toInt)
        case Array("up", i)      => (pos, depth, aim - i.toInt)
    }
    pos * depth

  println(part1) // 1488669
  println(part2) // 1176514794
