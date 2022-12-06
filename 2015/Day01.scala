@main def day01() =
  val instructions = input(1).next.view.map {
    case '(' => 1
    case ')' => -1
  }

  def part1 = instructions.sum

  def part2 =
    var sum = 0
    instructions.indexWhere { i =>
      sum += i
      sum == -1
    } + 1

  println(part1) // 74
  println(part2) // 1795
