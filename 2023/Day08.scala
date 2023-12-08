import scala.annotation.tailrec

@main def day08() =
  type Turn = String => String

  val document = input(8)
  val t = document.next
  document.next

  val extractor = """(\w+)""".r
  val network = document.map { line =>
    val Seq(s, l, r) = extractor.findAllIn(line).toSeq
    s -> (l, r)
  }.toMap

  val l: Turn = network(_).head
  val r: Turn = network(_).last

  val instructions = t.map {
    case 'L' => l
    case 'R' => r
  }.toSeq

  @tailrec
  def navigate(node: String, end: String => Boolean, turns: Seq[Turn] = instructions, acc: Int = 0): Long =
    if (end(node)) acc
    else
      val tail = if (turns.tail.isEmpty) instructions else turns.tail
      navigate(turns.head(node), end, tail, acc + 1)

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
  def lcm(a: Long, b: Long) = a * b / gcd(a, b)

  def part1 = navigate("AAA", "ZZZ".==)

  def part2 = network.keys
    .filter(_.last == 'A')
    .map(navigate(_, _.last == 'Z'))
    .reduce(lcm)

  println(part1) // 19637
  println(part2) // 8811050362409
