import scala.collection.mutable

@main def day12() =
  def records = input(12).map { line =>
    val Array(springs, damaged) = line.split(' ')
    springs -> damaged.split(',').map(_.toInt).toSeq
  }

  val memo = mutable.Map.empty[(String, Seq[Int]), Long]
  def arrangements(springs: String, damaged: Seq[Int], acc: Long = 0): Long =
    val s = springs.dropWhile('.'.==)
    memo.getOrElseUpdate(s -> damaged,
      if (damaged.isEmpty) acc + (if (s.contains('#')) 0 else 1)
      else if (s.isEmpty) acc
      else if (s.head == '?')
        arrangements("." + s.tail, damaged, acc) +
        arrangements("#" + s.tail, damaged, acc)
      else
        val (init, tail) = s.span('#'.==)
        val r = damaged.head - init.size
        (r.sign, tail.headOption) match
          case (0, Some('.') | None) => arrangements(tail, damaged.tail, acc)
          case (0, Some('?')) => arrangements(tail.tail, damaged.tail, acc)
          case (1, Some('?')) => arrangements("#" + tail.tail, r +: damaged.tail, acc)
          case _ => acc
    )

  def part1 = records.map(arrangements(_, _)).sum

  def part2 = records.map { (springs, damaged) =>
    val s = Seq.fill(5)(springs).mkString("?")
    val d = Seq.fill(5)(damaged).flatten
    arrangements(s, d)
  }.sum

  println(part1) // 7251
  println(part2) // 2128386729962
