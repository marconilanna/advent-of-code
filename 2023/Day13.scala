import scala.annotation.tailrec

@main def day13() =
  type Mirror = IndexedSeq[IndexedSeq[Char]]

  val mirrors =
    val (acc, aux) = input(13).foldLeft(Seq.empty[Mirror], IndexedSeq.empty: Mirror) {
      case ((acc, aux), line) =>
        if (line.isEmpty) (aux.reverse +: acc, IndexedSeq.empty)
        else (acc, line.toIndexedSeq +: aux)
    }
    aux.reverse +: acc

  def reflection(smudges: Int) =
    @tailrec
    def aux(mirror: Mirror, line: Int = 1): Option[Int] =
      val r = mirror.size - line
      if (r == 0) None
      else if ((0 until line.min(r)).map { i =>
        mirror(line - i - 1).zip(mirror(line + i)).count(_ != _)
      }.sum == smudges) Option(line)
      else aux(mirror, line + 1)

    mirrors.map { mirror =>
      aux(mirror.transpose).orElse(
      aux(mirror).map(100.*)).get
    }.sum

  def part1 = reflection(0)

  def part2 = reflection(1)

  println(part1) // 30158
  println(part2) // 36474
