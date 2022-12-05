import scala.collection.mutable

@main def day01() =
  def part1 = input(1).map(_.toInt).sum

  def part2 =
    val frequencies = mutable.Set(0)
    var frequency = 0
    while input(1).find { change =>
        frequency = frequency + change.toInt
        !frequencies.add(frequency)
      }.isEmpty
    do ()
    frequency

  println(part1) // 595
  println(part2) // 80598
