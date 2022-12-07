@main def day07() =
  val sizes =
    val lines = input(7).drop(1).filter(l => l.head != 'd' && l != "$ ls")

    def aux(acc: Seq[Int]): Seq[Int] =
      if (lines.hasNext)
        val line = lines.next
        line.head match
          case '$' => line.last match
            case '.' => acc
            case _ =>
              val subFolder = aux(0 +: acc.tail)
              val size = acc.head + subFolder.head
              aux(size +: subFolder)
          case _ =>
            val size = acc.head + line.takeWhile(_ != ' ').toInt
            aux(size +: acc.tail)
      else acc

    aux(Seq(0))

  def part1 = sizes.filter(_ <= 100000).sum

  def part2 =
    val target = sizes.head - 40000000
    sizes.filter(_ >= target).min

  println(part1) // 1555642
  println(part2) // 5974547
