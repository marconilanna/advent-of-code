import scala.annotation.tailrec

@main def day13() =
  val parser = """(\d+|\[|\])""".r
  def parse(s: String) = parser.findAllIn(s).toSeq
  def enlist(l: Seq[String]) = "[" +: l.head +: "]" +: l.tail
  def packets = input(13).filterNot(_.isEmpty).map(parse)

  @tailrec
  def inOrder(left: Seq[String], right: Seq[String]): Boolean =
    val (l, r) = (left.head(0), right.head(0))
    if (l.isDigit && r.isDigit)
      val (i, j) = (left.head.toInt, right.head.toInt)
      if (i == j) inOrder(left.tail, right.tail)
      else i < j
    else if (l.isDigit) inOrder(enlist(left), right)
    else if (r.isDigit) inOrder(left, enlist(right))
    else if (l == r) inOrder(left.tail, right.tail)
    else l == ']'

  def part1 = packets.grouped(2).zip(LazyList.from(1)).filter { case (p, _) =>
    inOrder(p(0), p(1))
  }.map(_._2).sum

  def part2 =
    val p = packets.toSeq.sortWith(inOrder)
    val a = p.indexWhere(!inOrder(_, parse("[[2]]"))) + 1
    val b = p.indexWhere(!inOrder(_, parse("[[6]]")), a) + 2
    a * b

  println(part1) // 5196
  println(part2) // 22134
