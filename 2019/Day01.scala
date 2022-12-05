import scala.annotation.tailrec

@main def day01() =
  def fuel(mass: Int) = mass / 3 - 2

  @tailrec
  def refuel(mass: Int, acc: Int = 0): Int =
    val f = fuel(mass)
    if (f > 0) refuel(f, acc + f)
    else acc

  def part1 = input(1).map(e => fuel(e.toInt)).sum

  def part2 = input(1).map(e => refuel(e.toInt)).sum

  println(part1) // 3167282
  println(part2) // 4748063
