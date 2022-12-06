@main def day02() =
  def part1 = input(2).foldLeft(IndexedSeq.empty[Int]) { case (acc, line) =>
    acc :+ line.foldLeft(acc.lastOption.getOrElse(5)) { case (button, move) =>
      move match
        case 'U' =>
          button match
            case 1 | 2 | 3 => button
            case _         => button - 3
        case 'D' =>
          button match
            case 7 | 8 | 9 => button
            case _         => button + 3
        case 'R' =>
          button match
            case 3 | 6 | 9 => button
            case _         => button + 1
        case 'L' =>
          button match
            case 1 | 4 | 7 => button
            case _         => button - 1
    }
  }.mkString

  def part2 = input(2).foldLeft(IndexedSeq.empty[Int]) { case (acc, line) =>
    acc :+ line.foldLeft(acc.lastOption.getOrElse(5)) { case (button, move) =>
      move match
        case 'U' =>
          button match
            case 1 | 2 | 4 | 5 | 9 => button
            case 3 | 13            => button - 2
            case _                 => button - 4
        case 'D' =>
          button match
            case 5 | 9 | 10 | 12 | 13 => button
            case 1 | 11               => button + 2
            case _                    => button + 4
        case 'R' =>
          button match
            case 1 | 4 | 9 | 12 | 13 => button
            case _                   => button + 1
        case 'L' =>
          button match
            case 1 | 2 | 5 | 10 | 13 => button
            case _                   => button - 1
    }
  }.map(_.toHexString).mkString

  println(part1) // 61529
  println(part2) // c2c28
