import scala.annotation.tailrec

@main def day01() =
  val entries = input(1).map(_.toInt).toIndexedSeq.sorted
  val target = 2020

  def part1 =
    @tailrec
    def aux(op1: IndexedSeq[Int], op2: IndexedSeq[Int]): Int =
      if (op2.isEmpty) aux(op1.tail, op1.drop(2))
      else
        val s = op2.size / 2
        val a = op2(s)
        val b = op1.head
        val sum = a + b
        if (sum == target) a * b
        else if (sum > target) aux(op1, op2.take(s))
        else aux(op1, op2.drop(s + 1))

    aux(entries, entries.tail)

  def part2 =
    @tailrec
    def aux(op1: IndexedSeq[Int], op2: IndexedSeq[Int], op3: IndexedSeq[Int]): Int =
      if (op2.isEmpty) aux(op1.tail, op1.drop(2), op1.drop(3))
      else if (op3.isEmpty) aux(op1, op2.tail, op2.drop(2))
      else
        val s = op3.size / 2
        val a = op3(s)
        val b = op2.head
        val c = op1.head
        val sum = a + b + c
        if (sum == target) a * b * c
        else if (sum > target) aux(op1, op2, op3.take(s))
        else aux(op1, op2, op3.drop(s + 1))

    aux(entries, entries.tail, entries.drop(2))

  println(part1) // 158916
  println(part2) // 165795564
