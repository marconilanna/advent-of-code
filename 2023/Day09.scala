import scala.annotation.tailrec

@main def day09() =
  def report = input(9).map(_.split(' ').map(_.toInt).toIndexedSeq)

  @tailrec
  def aux(diff: Seq[Int], acc: Int = 0): Int =
    if (diff.forall(0.==)) acc
    else aux(diff.sliding(2).map(_.reduce(_ - _)).toSeq, diff.head + acc)

  def extrapolate(f: Seq[Int] => Seq[Int]) = report.map(h => aux(f(h))).sum

  def part1 = extrapolate(_.reverse)

  def part2 = extrapolate(identity)

  println(part1) // 1969958987
  println(part2) // 1068
