import scala.annotation.tailrec
import scala.collection.mutable

@main def day15() =
  val parser = """(-?\d+)""".r
  def parse(line: String) = parser.findAllIn(line).map(_.toInt).toSeq

  @tailrec
  def collapse(ranges: Seq[Range], acc: Seq[Range]): Seq[Range] = ranges match
    case Seq(b, tail: _*) if b.start <= acc(0).end =>
      val r = acc(0).start to acc(0).end.max(b.end)
      collapse(tail, r +: acc.tail)
    case Seq(b, tail: _*) =>
      collapse(tail, b +: acc)
    case _ => acc

  def part1 =
    val row = 2000000
    val beacons = mutable.Set.empty[Int]
    val r = input(15).map(parse).flatMap { case Seq(a, b, x, y) =>
      if (y == row) beacons += x
      val d = (a - x).abs + (b - y).abs - (row - b).abs
      if (d > 0) Option((a - d) to (a + d))
      else None
    }.toSeq.sortBy(_.start)
    val ranges = collapse(r.tail, Seq(r.head))
    ranges.view.map(_.size).sum - beacons.count(b => ranges.exists(_.contains(b)))

  println(part1) // 5147333
