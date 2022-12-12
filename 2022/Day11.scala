import scala.collection.mutable

@main def day11() =
  def parse = input(11).grouped(7).map { lines =>
    def last(s: String, c: Char) = s.lastIndexOf(c) + 1
    def split(s: String) = s.splitAt(last(s, ' '))
    def drop(s: String, c: Char) = s.drop(last(s, c))
    def int(i: Int) = drop(lines(i), ' ').toInt

    val items = drop(lines(1), ':').split(',').map(_.trim.toLong)
    val op = {
      val (op, value) = split(lines(2))
      def input(f: Long => Long => Long) = if (value.head.isDigit) f(value.toLong) else (a: Long) => f(a)(a)
      op(op.size - 2) match
        case '+' => input(_.+)
        case '*' => input(_.*)
    }
    Monkey(items.toBuffer, op, int(3), level => if (level % int(3) == 0) int(4) else int(5))
  }

  def monkeyBusiness(rounds: Int, wl: Seq[Long] => Long => Long) =
    val monkeys = parse.toIndexedSeq
    val worryLevel = wl(monkeys.map(_.divisor))
    val count = mutable.IndexedSeq.fill(monkeys.size)(0)
    (1 to rounds).foreach { _ =>
      monkeys.zipWithIndex.foreach { case (monkey, i) =>
        monkey.items.foreach { item =>
          val level = worryLevel(monkey.op(item))
          val next = monkey.test(level)
          monkeys(next).items.append(level)
        }
        count(i) += monkey.items.size
        monkey.items.clear
      }
    }
    val (a, b) = count.foldLeft(0, 0) { case ((a, b), c) =>
      (a.max(c), a.min(c).max(b))
    }
    a.toLong * b

  def part1 = monkeyBusiness(20, _ => _ / 3)

  def part2 = monkeyBusiness(10000, s => _ % s.product)

  println(part1) // 55944
  println(part2) // 15117269860

case class Monkey(items: mutable.Buffer[Long], op: Long => Long, divisor: Int, test: Long => Int)
