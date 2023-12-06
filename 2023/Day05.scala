import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

@main def day05() =
  type Almanac = (String, SortedSet[Conversion])
  // NumericRange.Exclusive[Long] seems to have serious performance issues
  case class Range(from: Long, to: Long)

  object Range:
    def apply(n: Long): Range = Range(n, n + 1)
    def apply(s: Seq[Long]): Range = Range(s.head, s.head + s.last)

  case class Conversion(range: Range, delta: Long):
    /*
               |~~~~~|
        |---|
        |------|
        |---------|           t(r1, p2) -> (p2, r2)
        |------------|        t(r)
        |---------------|     t(r)
               |--|           t(r1, p2) -> (p2, r2)
               |-----|        t(r)
               |--------|     t(r)
                  |-|         (r1, p1) -> (p1, r2)
                  |--|        (r1, p1) -> (p1, r2)
                  |-----|     (r1, p1) -> (p1, r2)
                     |--|     r
                        |--|  r
     */
    def pivot(r: Range) = range.to > r.from && range.from < r.to

    def convert(r: Range): (Range, Seq[Range]) =
      def map(from: Long, to: Long) = Range(from + delta, to + delta)

      if (range.from <= r.from && range.to < r.to)
        (map(r.from, range.to), Seq(r.copy(from = range.to)))
      else if (range.from > r.from)
        (r.copy(to = r.from), Seq(r.copy(from = range.from)))
      else
        (map(r.from, r.to), Seq.empty)

  object Conversion:
    implicit val ordering: Ordering[Conversion] = Ordering.by(_.range.to)
    def apply(ds: Long, ss: Long, l: Long): Conversion = Conversion(Range(ss, ss + l), ds - ss)

  val extractor = """(\w+)-to-(\w+)""".r.unanchored

  val source = input(5)
  val seeds = source
    .next
    .split(':')
    .last
    .split(' ')
    .tail
    .map(_.toLong)
    .toSeq

  val almanac = source.foldLeft("", Map.empty[String, Almanac]) { case ((source, maps), line) =>
    line match
      case line if line.headOption.exists(_.isDigit) =>
        val Array(ds, ss, l) = line.split(' ').map(_.toLong)
        val conversion = Conversion(ds, ss, l)
        val (destination, conversions) = maps(source)
        (source, maps + (source -> (destination, conversions + conversion)))

      case extractor(source, destination) =>
        (source, maps + (source -> (destination, SortedSet.empty)))

      case _ => ("", maps)
  }._2

  @tailrec
  def convert(ranges: Seq[Range], source: String = "seed"): Long =
    if (!almanac.contains(source))
      ranges.view.map(_.from).min
    else
      val (destination, conversions) = almanac(source)

      @tailrec
      def aux(ranges: Seq[Range], done: Seq[Range] = Seq.empty): Seq[Range] =
        if (ranges.isEmpty) done
        else
          val range = ranges.head
          val (converted, tail) = conversions
            .find(_.pivot(range))
            .map(_.convert(range))
            .getOrElse(range, Seq.empty)
          aux(tail ++ ranges.tail, converted +: done)

      convert(aux(ranges), destination)

  def part1 = convert(seeds
    .map(Range(_))
  )

  def part2 = convert(seeds
    .grouped(2)
    .map(Range(_))
    .toSeq
  )

  println(part1) // 289863851
  println(part2) // 60568880
